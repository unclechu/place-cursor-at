#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -E "let ghc = n.haskellPackages.ghcWithPackages (p: [p.base-unicode-symbols p.X11]); d = n.mkShell { buildInputs = [ghc]; }; n = import (fetchTarball { url = \"https://github.com/NixOS/nixpkgs/archive/a3fa481cb683d619ab9e1a64877f3c0c5fd24f40.tar.gz\"; sha256 = \"0y5dzi3npv13vyacmb4q93j0cpg6gjgvylq4ckjjvcb6szdsizqi\"; }) {}; in d"

{-# OPTIONS_GHC -Wall -fprint-potential-instances #-}
{-# LANGUAGE UnicodeSyntax, MultiWayIf, ViewPatterns, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

import Prelude.Unicode

import Data.Bits ((.|.))
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe
import Numeric.Natural
import Text.ParserCombinators.ReadP (satisfy)
import Text.Read (ReadPrec, Read (readPrec), lift, choice, readMaybe)

import Control.Applicative ((<|>))
import Control.Arrow ((***), (&&&))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (try)
import Control.Monad (forever, forM_, foldM, when, void)

import System.Environment (getArgs)

import Graphics.X11.Xlib

import Graphics.X11.Xlib.Extras ( SizeHints (..)
                                , pMinSizeBit
                                , pMaxSizeBit
                                , changeProperty8
                                , propModeReplace
                                , queryTree
                                , getTextProperty
                                , TextProperty (tp_value)
                                , killClient
                                )

import Graphics.X11.Xinerama ( xineramaQueryScreens
                             , XineramaScreenInfo (..)
                             )

import Foreign.C.String (castCharToCChar, peekCString)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (poke))

foreign import ccall unsafe "XlibExtras.h XSetWMNormalHints"
  xSetWMNormalHints ∷ Display → Window → Ptr SizeHints → IO ()

foreign import ccall unsafe "XlibExtras.h XSetWMSizeHints"
  xSetWMSizeHints ∷ Display → Window → Ptr SizeHints → Int → IO ()


s, w, h, fontSize, offsetPercent ∷ Num a ⇒ a
s = 40; w = s; h = s
fontSize      = 32
offsetPercent = 10


fontQ ∷ String
fontQ = "-*-terminus-bold-r-normal-*-" ⧺ show (fontSize ∷ ℤ) ⧺ "-*-*-*-*-*-*-*"


windowClassName ∷ String
windowClassName = "place-cursor-at"


data Pos = PosLT | PosCT | PosRT
         | PosLC | PosCC | PosRC
         | PosLB | PosCB | PosRB
           deriving (Eq, Show)

instance Read Pos where
  readPrec = go where
    go = choice $ lift <$> x
    insStr a b = void $ satisfy ((a ≡) ∘ toUpper) *> satisfy ((b ≡) ∘ toUpper)

    x =
      [ PosLT <$ insStr 'L' 'T', PosCT <$ insStr 'C' 'T', PosRT <$ insStr 'R' 'T'
      , PosLC <$ insStr 'L' 'C', PosCC <$ insStr 'C' 'C', PosRC <$ insStr 'R' 'C'
      , PosLB <$ insStr 'L' 'B', PosCB <$ insStr 'C' 'B', PosRB <$ insStr 'R' 'B'
      ]


positions ∷ [ (Pos, (String, KeyCode)) ]
positions = [ (PosLT, ("Q", 24)), (PosCT, ("W", 25)), (PosRT, ("E", 26))
            , (PosLC, ("A", 38)), (PosCC, ("S", 39)), (PosRC, ("D", 40))
            , (PosLB, ("Z", 52)), (PosCB, ("X", 53)), (PosRB, ("C", 54))
            ]


-- | Command-line arguments
data Argv
   = Argv
   { argvOnDisplay  ∷ Maybe SpecificScreenNumber
   -- ^ Jump to specific display
   , argvToPosition ∷ Maybe Pos
   -- ^ Jump to specific position on screen (GUI wont be shown)
   } deriving (Show, Eq)

emptyArgv ∷ Argv
emptyArgv = Argv Nothing Nothing


newtype SpecificScreenNumber
      = SpecificScreenNumber { fromSpecificScreenNumber ∷ Natural }
        deriving newtype (Eq, Enum)

instance Show SpecificScreenNumber where
  show = fromSpecificScreenNumber • succ • show

instance Read SpecificScreenNumber where
  readPrec =
    (readPrec ∷ ReadPrec Natural) >>= \x →
      if x ≡ 0
      then fail $ "Incorrect screen number " ⧺ show x ⧺ " (must start with 1)"
      else pure $ pred $ SpecificScreenNumber x


main ∷ IO ()
main = do
  doneHandler ← mkDoneHandler
  (dpy, rootWnd) ← (id &&& defaultRootWindow) <$> openDisplay ""
  killPreviousInstanceIfExists dpy

  (xsn, justGoToPos') ←
    let
      reducer argv x =
        case (readMaybe x <&> \x' → argv { argvOnDisplay  = Just x' })
         <|> (readMaybe x <&> \x' → argv { argvToPosition = Just x' })
          of Just updatedArgv → pure updatedArgv
             Nothing → fail $ "Unexpected argument: " ⧺ show x
    in
      (getArgs >>= foldM reducer emptyArgv) <&> (argvOnDisplay &&& argvToPosition)

  xsi ← getScreenInfo dpy xsn

  seq xsi $
    when (isNothing justGoToPos') $
      closeDisplay dpy

  let xX, xY, xW, xH ∷ ℚ
      relativeX, relativeY ∷ Pos → ℚ

      from f = fromIntegral $ f xsi
      xX = from xsi_x_org
      xY = from xsi_y_org
      xW = from xsi_width
      xH = from xsi_height

      relativeX pos
        | pos ∈ [PosLT, PosLC, PosLB] = xW ⋅ offsetPercent ÷ 100
        | pos ∈ [PosCT, PosCC, PosCB] = xW ÷ 2
        | pos ∈ [PosRT, PosRC, PosRB] = xW ⋅ (100 - offsetPercent) ÷ 100
        | otherwise = error "Unexpected behavior"

      relativeY pos
        | pos ∈ [PosLT, PosCT, PosRT] = xH ⋅ offsetPercent ÷ 100
        | pos ∈ [PosLC, PosCC, PosRC] = xH ÷ 2
        | pos ∈ [PosLB, PosCB, PosRB] = xH ⋅ (100 - offsetPercent) ÷ 100
        | otherwise = error "Unexpected behavior"

      toPosition ∷ ℚ → Position
      toPosition = read ∘ show ∘ (round ∷ ℚ → ℤ)

      windows ∷ [(String, (Position, Position))]
      windows = flip map positions $ \(pos, (text, _)) →

        let x, y ∷ ℚ
            x = xX + relativeX pos - (w ÷ 2)
            y = xY + relativeY pos - (h ÷ 2)

         in (text, (toPosition x, toPosition y))

      places ∷ [((Pos, KeyCode), (Position, Position))]
      places = flip map positions $ \(pos, (_, keyCode)) →

        let x, y ∷ ℚ
            x = xX + relativeX pos
            y = xY + relativeY pos

         in ((pos, keyCode), (toPosition x, toPosition y))

  case justGoToPos' of
       Just pos →
         case lookup pos $ places <&> \((pos', _), coords) → (pos', coords) of
              Just (x, y) → do
                placeCursorAt dpy rootWnd x y
                closeDisplay dpy

              Nothing → fail $ "Unexpectedly fail to find a 'place' by " ⧺ show pos

       Nothing → do
         let places' = places <&> \((_, keyCode), coords) → (keyCode, coords)
         forM_ windows $ forkIO ∘ windowInstance (doneWithIt doneHandler) places'
         waitBeforeItIsDone doneHandler


-- | Kill previous instance of *place-cursor-at*.
--
-- FIXME In some cases it fails the killer with this exception in the log:
--
-- @
-- X Error of failed request:  BadValue (integer parameter out of range for operation)
--   Major opcode of failed request:  113 (X_KillClient)
--   Value in failed request:  0x6000002
--   Serial number of failed request:  259
--   Current serial number in output stream:  260
-- @
--
-- I have no idea what the heck is this. I tried many ways to fix this with no success.
killPreviousInstanceIfExists ∷ Display → IO ()
killPreviousInstanceIfExists dpy = go where
  go = traverseChildWindows (defaultRootWindow dpy)
  kill = killClient dpy

  traverseChildWindows ∷ Window → IO ()
  traverseChildWindows wnd = killEmRecursively where
    killEmRecursively =
      getAllWindowsList wnd
      >>= foldM reducer mempty
      >>= (mapM_ kill *** mapM_ traverseChildWindows) • uncurry (>>)

    reducer ∷ acc ~ ([Window], [Window]) ⇒ acc → Window → IO acc
    reducer acc x = f where
      f = isPlaceCursorAtWindow x <&> \is → (x `appendIf` is *** x `appendIf` not is) acc
      appendIf wndToAppend = bool id (⧺ [wndToAppend])

  getAllWindowsList ∷ Window → IO [Window]
  getAllWindowsList wnd = queryTree dpy wnd <&> \(_, _, x) → x

  isPlaceCursorAtWindow ∷ Window → IO 𝔹
  isPlaceCursorAtWindow wnd = x where
    x = try matchByWindowClass <&> either (const False ∷ IOError → 𝔹) id

    matchByWindowClass =
      getTextProperty dpy wnd wM_CLASS
      >>= tp_value • peekCString
      >>= (≡ windowClassName) • pure


-- | Get info about screen either under cursor or specified by an argument.
getScreenInfo ∷ Display → Maybe SpecificScreenNumber → IO XineramaScreenInfo
getScreenInfo dpy specificScreen = go where
  xineramaFailureMsg
    = "Could not obtain Xinerama screens information, "
    ⧺ "check that libXinerama dependency is installed "
    ⧺ "and Xinerama X11 extension is active!"

  isSpecifiedScreen ∷ SpecificScreenNumber → XineramaScreenInfo → 𝔹
  isSpecifiedScreen (SpecificScreenNumber (toInteger • fromInteger → n)) =
    xsi_screen_number • succ • (≡ n)

  isScreenUnderCursor ∷ (ℤ, ℤ) → XineramaScreenInfo → 𝔹
  isScreenUnderCursor (mX, mY) screenInfo = x where
    f        = fromIntegral
    (x1, y1) = (f (xsi_x_org screenInfo),      f (xsi_y_org screenInfo))
    (x2, y2) = (x1 + f (xsi_width screenInfo), y1 + f (xsi_height screenInfo))
    x        = (mX ≥ x1 ∧ mY ≥ y1) ∧ (mX < x2 ∧ mY < y2)

  go = do
    screens ←
      xineramaQueryScreens dpy >>=
        maybe (fail xineramaFailureMsg) pure

    (predicateFn ∷ XineramaScreenInfo → 𝔹, failureMsg ∷ String) ←
      case specificScreen of
           Nothing → do
             mouseCoords ← mousePos dpy (defaultRootWindow dpy) <&> (fromIntegral *** fromIntegral)

             let
               failureMsg
                 = "Could not find a screen which is under cursor, something went wrong "
                 ⧺ "(mouse position: " ⧺ show mouseCoords ⧺ ", screens: " ⧺ show screens ⧺ ")"

             pure (isScreenUnderCursor mouseCoords, failureMsg)

           Just screenNum →
             let
               failureMsg
                 = "Could not find screen by number " ⧺ show (succ screenNum) ⧺ ", "
                 ⧺ "specified screen number must be out of range "
                 ⧺ "(screens: " ⧺ show screens ⧺ ")"
             in
               pure (isSpecifiedScreen screenNum, failureMsg)

    maybe (fail failureMsg) pure (find predicateFn screens)


windowInstance ∷ IO ()
               → [(KeyCode, (Position, Position))]
               → (String, (Position, Position))
               → IO ()

windowInstance done places (text, (wndX, wndY)) = do

  dpy ← openDisplay ""

  let
    rootWnd = defaultRootWindow dpy
    screen  = defaultScreen     dpy
    gc      = defaultGC         dpy screen
    blackPx = blackPixel        dpy screen
    whitePx = whitePixel        dpy screen

  fontStruct ← loadQueryFont dpy fontQ
  setFont dpy gc $ fontFromFontStruct fontStruct
  setForeground dpy gc whitePx

  let
    placeAt ∷ Position → Position → IO ()
    placeAt = placeCursorAt dpy rootWnd

  wnd ← createSimpleWindow dpy rootWnd 0 0 w h 0 whitePx blackPx
  shPtr ← allocSH
  xSetWMNormalHints dpy wnd shPtr
  xSetWMSizeHints dpy wnd shPtr $ pMinSizeBit .|. pMaxSizeBit

  storeName dpy wnd $ "Place Cursor At [" ⧺ text ⧺ "]"
  changeProperty8 dpy wnd wM_CLASS sTRING propModeReplace $ castCharToCChar <$> windowClassName

  mapWindow dpy wnd
  placeWindowAt dpy wnd wndX wndY

  selectInput dpy wnd $ keyPressMask .|. exposureMask
  () <$ allocaXEvent (forever ∘ evLoop done dpy wnd gc fontStruct placeAt text places)


allocSH ∷ IO (Ptr SizeHints)
allocSH = go where
  go = malloc <&> unsafeForeignPtrToPtr >>= \ptr → ptr <$ poke ptr sh

  malloc ∷ IO (ForeignPtr SizeHints)
  malloc = mallocForeignPtr

  sh ∷ SizeHints
  sh = SizeHints { sh_min_size    = Just (w, h)
                 , sh_max_size    = Just (w, h)
                 , sh_resize_inc  = Nothing
                 , sh_aspect      = Nothing
                 , sh_base_size   = Nothing
                 , sh_win_gravity = Nothing
                 }


mousePos ∷ Display → Window → IO (ℤ, ℤ)
mousePos dpy wnd = f <$> queryPointer dpy wnd
  where f (_, _, _, rootX, rootY, _, _, _) = (toInteger rootX, toInteger rootY)


placeWindowAt ∷ Display → Window → Position → Position → IO ()
placeWindowAt dpy wnd x y = moveResizeWindow dpy wnd x y w h

placeCursorAt ∷ Display → Window → Position → Position → IO ()
placeCursorAt dpy wnd x y = f where f = warpPointer dpy wnd wnd 0 0 0 0 x y


evLoop ∷ IO ()
       → Display → Window → GC → FontStruct
       → (Position → Position → IO ())
       → String
       → [(KeyCode, (Position, Position))]
       → XEventPtr
       → IO ()

evLoop done dpy wnd gc fontStruct placeAt text places evPtr = do
  nextEvent dpy evPtr
  evType ← get_EventType evPtr
  let getKeyCode (_, _, _, _, _, _, _, _, keyCode, _) = keyCode

  if
   | evType ≡ keyPress → get_KeyEvent evPtr <&> getKeyCode >>= handleKey done placeAt places text
   | evType ≡ expose   → draw dpy wnd gc fontStruct text
   | otherwise         → pure ()


handleKey ∷ IO ()
          → (Position → Position → IO ())
          → [(KeyCode, (Position, Position))]
          → String
          → KeyCode
          → IO ()

handleKey done placeAt places text keyCode
  | keyCode ≡ 9  = done -- Escape
  | keyCode ≡ 36 = handleKey done placeAt places (⊥) currentWindowKeyCode -- Enter
  | isJust found = uncurry placeAt (fromJust found) >> done
  | otherwise    = pure ()
  where
    found = snd <$> find ((≡ keyCode) ∘ fst) places
    currentWindowKeyCode = fromJust $ snd ∘ snd <$> find ((≡ text) ∘ fst ∘ snd) positions


draw ∷ Display → Window → GC → FontStruct → String → IO ()
draw dpy wnd gc fontStruct text = go where
  go = drawString dpy wnd gc textXPos textYPos text
  tw = textWidth fontStruct text

  textXPos, textYPos ∷ Position

  textXPos = x where
    x = read $ show (round $ wndCenter - textCenter ∷ Int)
    wndCenter = fromIntegral (w ∷ ℤ) ÷ 2 ∷ Float
    textCenter = fromIntegral tw ÷ 2 ∷ Float

  textYPos = x where
    x = read $ show (round $ wndCenter + textCenter ∷ Int)
    wndCenter = fromIntegral (h ∷ ℤ) ÷ 2 ∷ Float
    textCenter = fontSize ÷ 4 ∷ Float


data DoneApi = DoneApi
   { doneWithIt         ∷ IO ()
   , waitBeforeItIsDone ∷ IO ()
   }

mkDoneHandler ∷ IO DoneApi
mkDoneHandler = newEmptyMVar <&> \mvar → DoneApi
  { doneWithIt         = putMVar mvar ()
  , waitBeforeItIsDone = readMVar mvar
  }


(•) ∷ (a → b) → (b → c) → a → c
(•) = flip (∘)
infixl 9 •
{-# INLINE (•) #-}

type 𝔹 = Bool
