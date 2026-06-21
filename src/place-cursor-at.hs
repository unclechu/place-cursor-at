#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -E "let ghc = n.haskellPackages.ghcWithPackages (p: [p.base-unicode-symbols p.X11]); d = n.mkShell { buildInputs = [ghc]; }; n = import (fetchTarball { url = \"https://github.com/NixOS/nixpkgs/archive/a0374025a863d007d98e3297f6aa46cc3141c2f0.tar.gz\"; sha256 = \"14c0hqipkcm2iqi5ybjpx4xcvwxjp3sx3rfgb69dv83h0gm1crgn\"; }) {}; in d"

-- Author: Viacheslav Lotsmanov
-- License: GNU/GPLv3 https://raw.githubusercontent.com/unclechu/place-cursor-at/master/LICENSE

{-# OPTIONS_GHC -Wall -fprint-potential-instances #-}
{-# LANGUAGE UnicodeSyntax, BangPatterns, MultiWayIf, ViewPatterns, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE LambdaCase, DerivingStrategies, GeneralizedNewtypeDeriving #-}

import Prelude.Unicode ((∘), (÷), (≡), (⧺), (∧), (≥))

import Data.Bifunctor (first)
import Data.Bits ((.|.))
import Data.Char (toUpper)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.List (find)
import qualified Numeric.Natural as N
import Text.ParserCombinators.ReadP (satisfy)
import Text.Read (ReadPrec, Read (readPrec), lift, choice, readMaybe)
import Control.Applicative ((<|>))
import Control.Arrow ((***), (&&&))
import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (forever, forM_, foldM, void)
import System.Environment (getArgs)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibExtras
import qualified Graphics.X11.Xinerama as Xinerama
import Foreign.C.String (castCharToCChar, peekCString)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (poke))

foreign import ccall unsafe "XlibExtras.h XSetWMNormalHints"
  xSetWMNormalHints ∷ Xlib.Display → Xlib.Window → Ptr XlibExtras.SizeHints → IO ()

foreign import ccall unsafe "XlibExtras.h XSetWMSizeHints"
  xSetWMSizeHints ∷ Xlib.Display → Xlib.Window → Ptr XlibExtras.SizeHints → Int → IO ()


s, w, h, letterPaddingX, letterPaddingY, offsetPercent ∷ Num a ⇒ a
s = 40; w = s; h = s
letterPaddingX = 10; letterPaddingY = 10
offsetPercent = 10


windowClassName ∷ String
windowClassName = "place-cursor-at"


data Pos
   = PosLT | PosCT | PosRT
   | PosLC | PosCC | PosRC
   | PosLB | PosCB | PosRB
     deriving stock (Eq, Show, Enum, Bounded)

instance Read Pos where
  readPrec = go where
    go = choice $ lift <$> x
    insStr a b = void $ satisfy ((a ≡) ∘ toUpper) *> satisfy ((b ≡) ∘ toUpper)

    x =
      [ PosLT <$ insStr 'L' 'T', PosCT <$ insStr 'C' 'T', PosRT <$ insStr 'R' 'T'
      , PosLC <$ insStr 'L' 'C', PosCC <$ insStr 'C' 'C', PosRC <$ insStr 'R' 'C'
      , PosLB <$ insStr 'L' 'B', PosCB <$ insStr 'C' 'B', PosRB <$ insStr 'R' 'B'
      ]


data Letter
   = Q | W | E
   | A | S | D
   | Z | X | C
     deriving stock (Eq, Show)

letterToKeyCode ∷ Letter → Xlib.KeyCode
letterToKeyCode = \case
  Q → 24; W → 25; E → 26
  A → 38; S → 39; D → 40
  Z → 52; X → 53; C → 54

positionToLetter ∷ Pos → Letter
positionToLetter = \case
  PosLT → Q; PosCT → W; PosRT → E
  PosLC → A; PosCC → S; PosRC → D
  PosLB → Z; PosCB → X; PosRB → C


-- | Position relative to either X or Y axis
data AbstractPos
   = MinPos    -- ^ Left of Top
   | MiddlePos -- ^ Center
   | MaxPos    -- ^ Right or Bottom
     deriving stock (Eq, Show)

getAbstractPosX ∷ Pos → AbstractPos
getAbstractPosX = \case
  PosLT → MinPos;    PosLC → MinPos;    PosLB → MinPos
  PosCT → MiddlePos; PosCC → MiddlePos; PosCB → MiddlePos
  PosRT → MaxPos;    PosRC → MaxPos;    PosRB → MaxPos

getAbstractPosY ∷ Pos → AbstractPos
getAbstractPosY = \case
  PosLT → MinPos;    PosCT → MinPos;    PosRT → MinPos
  PosLC → MiddlePos; PosCC → MiddlePos; PosRC → MiddlePos
  PosLB → MaxPos;    PosCB → MaxPos;    PosRB → MaxPos

abstractPosToCoordinate ∷ Fractional a ⇒ a → AbstractPos → a
abstractPosToCoordinate x = \case
  MinPos    → x × offsetPercent ÷ 100
  MiddlePos → x ÷ 2
  MaxPos    → x × (100 - offsetPercent) ÷ 100


-- | Command-line arguments
data Argv
   = Argv
   { argvOnDisplay  ∷ Maybe SpecificScreenNumber
   -- ^ Jump to specific display
   , argvToPosition ∷ Maybe Pos
   -- ^ Jump to specific position on screen (GUI wont be shown)
   } deriving stock (Show, Eq)

emptyArgv ∷ Argv
emptyArgv = Argv Nothing Nothing


-- | Screen number that starts with 0.
--
-- N.B. In "Read" and "Show" instances it starts with 1.
-- When parsed or printed it either decrements or increments.
newtype SpecificScreenNumber
      = SpecificScreenNumber { fromSpecificScreenNumber ∷ N.Natural }
        deriving newtype (Eq, Enum)

-- | Prints wrapped number as a string.
--
-- N.B. But printed number increments, it starts with 1 (instead of 0).
instance Show SpecificScreenNumber where
  show = fromSpecificScreenNumber • succ • show

-- | It reads screen number as plain number that starts with 1.
--
-- N.B. Wrapped value is decremented.
instance Read SpecificScreenNumber where
  readPrec =
    (readPrec ∷ ReadPrec N.Natural) >>= \x →
      if x ≡ 0
      then fail $ unwords ["Incorrect screen number", show x, " (must start with 1)"]
      else pure ∘ pred ∘ SpecificScreenNumber $ x


main ∷ IO ()
main = do
  doneHandler ← mkDoneHandler
  (dpy, rootWnd) ← (id &&& Xlib.defaultRootWindow) <$> Xlib.openDisplay ""
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

  !xsi ← getScreenInfo dpy xsn

  let
    xX, xY, xW, xH ∷ Rational
    relativeX, relativeY ∷ Pos → Rational

    from f = fromIntegral $ f xsi
    xX = from Xinerama.xsi_x_org
    xY = from Xinerama.xsi_y_org
    xW = from Xinerama.xsi_width
    xH = from Xinerama.xsi_height
    relativeX = getAbstractPosX • abstractPosToCoordinate xW
    relativeY = getAbstractPosY • abstractPosToCoordinate xH

    toPosition ∷ Rational → Xlib.Position
    toPosition = fromInteger ∘ round

    windows ∷ [(Letter, (Xlib.Position, Xlib.Position))]
    windows = [minBound..maxBound] <&> \pos →
      let
        x, y ∷ Rational
        x = xX + relativeX pos - (w ÷ 2)
        y = xY + relativeY pos - (h ÷ 2)
      in
        (positionToLetter pos, (toPosition x, toPosition y))

    places ∷ [(Pos, (Xlib.Position, Xlib.Position))]
    places = [minBound..maxBound] <&> \pos →
      let
        x, y ∷ Rational
        x = xX + relativeX pos
        y = xY + relativeY pos
      in
        (pos, (toPosition x, toPosition y))

  case justGoToPos' of
       Just pos →
         case lookup pos places of
              Just (x, y) → do
                placeCursorAt dpy rootWnd x y
                Xlib.closeDisplay dpy

              Nothing → fail $ "Unexpectedly fail to find a 'place' by " ⧺ show pos

       Nothing → do
         Xlib.closeDisplay dpy
         let places' = places <&> first (letterToKeyCode ∘ positionToLetter)
         let done = doneWithIt doneHandler
         forM_ windows $ windowInstance (done (pure ())) places' • (`forkFinally` done)
         waitBeforeItIsDone doneHandler >>= either throwIO pure


-- | Kill previous instance of *place-cursor-at*
killPreviousInstanceIfExists ∷ Xlib.Display → IO ()
killPreviousInstanceIfExists dpy = go where
  go = findPreyToKill (Xlib.defaultRootWindow dpy) >>= mapM_ (XlibExtras.killClient dpy)

  findPreyToKill ∷ Xlib.Window → IO [Xlib.Window]
  findPreyToKill =
    ($ []) $ fix $ \again acc !wnd →
      isPlaceCursorAtWindow wnd >>= \case
        False → foldM again acc =<< getChildWindows wnd
        True → slurpAllChildren acc wnd
    where
      slurpAllChildren acc !wnd =
        foldM slurpAllChildren (wnd : acc) =<< getChildWindows wnd

  getChildWindows ∷ Xlib.Window → IO [Xlib.Window]
  getChildWindows wnd = XlibExtras.queryTree dpy wnd <&> \(_, _, x) → x

  isPlaceCursorAtWindow ∷ Xlib.Window → IO Bool
  isPlaceCursorAtWindow wnd = x where
    x = try matchByWindowClass <&> either (const False ∷ IOError → Bool) id

    matchByWindowClass =
      XlibExtras.getTextProperty dpy wnd Xlib.wM_CLASS
      >>= XlibExtras.tp_value • peekCString
      >>= (≡ windowClassName) • pure


-- | Get info about screen either under cursor or specified by an argument.
getScreenInfo ∷ Xlib.Display → Maybe SpecificScreenNumber → IO Xinerama.XineramaScreenInfo
getScreenInfo dpy specificScreen = go where
  xineramaFailureMsg = unwords
    [ "Could not obtain Xinerama screens information,"
    , "check that libXinerama dependency is installed"
    , "and Xinerama X11 extension is active!"
    ]

  isSpecifiedScreen ∷ SpecificScreenNumber → Xinerama.XineramaScreenInfo → Bool
  isSpecifiedScreen (SpecificScreenNumber (toInteger • fromInteger → n)) =
    Xinerama.xsi_screen_number • (≡ n)

  isScreenUnderCursor ∷ (Integer, Integer) → Xinerama.XineramaScreenInfo → Bool
  isScreenUnderCursor (mX, mY) screenInfo = x where
    f        = fromIntegral
    (x1, y1) = (f (Xinerama.xsi_x_org screenInfo),      f (Xinerama.xsi_y_org screenInfo))
    (x2, y2) = (x1 + f (Xinerama.xsi_width screenInfo), y1 + f (Xinerama.xsi_height screenInfo))
    x        = (mX ≥ x1 ∧ mY ≥ y1) ∧ (mX < x2 ∧ mY < y2)

  go = do
    screens ←
      Xinerama.xineramaQueryScreens dpy >>=
        maybe (fail xineramaFailureMsg) pure

    (predicateFn ∷ Xinerama.XineramaScreenInfo → Bool, failureMsg ∷ String) ←
      case specificScreen of
           Nothing → do
             mouseCoords ←
               mousePos dpy (Xlib.defaultRootWindow dpy) <&> (fromIntegral *** fromIntegral)

             let
               failureMsg = unwords
                 [ "Could not find a screen which is under cursor, something went wrong"
                 , "(mouse position:", show mouseCoords ⧺ ", screens:", show screens ⧺ ")"
                 ]

             pure (isScreenUnderCursor mouseCoords, failureMsg)

           Just screenNum →
             let
               failureMsg = unwords
                 [ "Could not find screen number", show screenNum
                 , "(by Xinerama screen number", show (fromSpecificScreenNumber screenNum) ⧺ "),"
                 , "specified screen number is probably out of range"
                 , "(screens:", show screens ⧺ ")"
                 ]
             in
               pure (isSpecifiedScreen screenNum, failureMsg)

    maybe (fail failureMsg) pure (find predicateFn screens)


windowInstance
  ∷ IO ()
  → [(Xlib.KeyCode, (Xlib.Position, Xlib.Position))]
  → (Letter, (Xlib.Position, Xlib.Position))
  → IO ()

windowInstance done places (letter, (wndX, wndY)) = do

  dpy ← Xlib.openDisplay ""

  let
    rootWnd = Xlib.defaultRootWindow dpy
    screen  = Xlib.defaultScreen     dpy
    gc      = Xlib.defaultGC         dpy screen
    blackPx = Xlib.blackPixel        dpy screen
    whitePx = Xlib.whitePixel        dpy screen

  Xlib.setLineAttributes dpy gc 3 0 0 0 -- Increase line thickness
  Xlib.setForeground dpy gc whitePx

  let
    placeAt ∷ Xlib.Position → Xlib.Position → IO ()
    placeAt = placeCursorAt dpy rootWnd

  wnd ← Xlib.createSimpleWindow dpy rootWnd 0 0 w h 0 whitePx blackPx
  shPtr ← allocSH
  xSetWMNormalHints dpy wnd shPtr
  xSetWMSizeHints dpy wnd shPtr $ XlibExtras.pMinSizeBit .|. XlibExtras.pMaxSizeBit

  Xlib.storeName dpy wnd $ "Place Cursor At [" ⧺ show letter ⧺ "]"
  XlibExtras.changeProperty8 dpy wnd Xlib.wM_CLASS Xlib.sTRING XlibExtras.propModeReplace $
    castCharToCChar <$> windowClassName

  Xlib.mapWindow dpy wnd
  placeWindowAt dpy wnd wndX wndY

  Xlib.selectInput dpy wnd $ Xlib.keyPressMask .|. Xlib.exposureMask
  () <$ Xlib.allocaXEvent (forever ∘ evLoop done dpy wnd gc placeAt letter places)


allocSH ∷ IO (Ptr XlibExtras.SizeHints)
allocSH = go where
  go = malloc >>= unsafeForeignPtrToPtr • \ptr → ptr <$ poke ptr sh

  malloc ∷ IO (ForeignPtr XlibExtras.SizeHints)
  malloc = mallocForeignPtr

  sh ∷ XlibExtras.SizeHints
  sh = XlibExtras.SizeHints
    { XlibExtras.sh_min_size    = Just (w, h)
    , XlibExtras.sh_max_size    = Just (w, h)
    , XlibExtras.sh_resize_inc  = Nothing
    , XlibExtras.sh_aspect      = Nothing
    , XlibExtras.sh_base_size   = Nothing
    , XlibExtras.sh_win_gravity = Nothing
    }


mousePos ∷ Xlib.Display → Xlib.Window → IO (Integer, Integer)
mousePos dpy wnd = f <$> Xlib.queryPointer dpy wnd
  where f (_, _, _, rootX, rootY, _, _, _) = (toInteger rootX, toInteger rootY)


placeWindowAt ∷ Xlib.Display → Xlib.Window → Xlib.Position → Xlib.Position → IO ()
placeWindowAt dpy wnd x y = Xlib.moveResizeWindow dpy wnd x y w h

placeCursorAt ∷ Xlib.Display → Xlib.Window → Xlib.Position → Xlib.Position → IO ()
placeCursorAt dpy wnd x y = f where f = Xlib.warpPointer dpy wnd wnd 0 0 0 0 x y


evLoop
  ∷ IO ()
  → Xlib.Display → Xlib.Window → Xlib.GC
  → (Xlib.Position → Xlib.Position → IO ())
  → Letter
  → [(Xlib.KeyCode, (Xlib.Position, Xlib.Position))]
  → Xlib.XEventPtr
  → IO ()

evLoop done dpy wnd gc placeAt letter places evPtr = do
  Xlib.nextEvent dpy evPtr
  evType ← Xlib.get_EventType evPtr
  let getKeyCode (_, _, _, _, _, _, _, _, keyCode, _) = keyCode

  if
   | evType ≡ Xlib.keyPress →
       Xlib.get_KeyEvent evPtr >>= getKeyCode • handleKey done placeAt places letter
   | evType ≡ Xlib.expose → draw dpy wnd gc letter
   | otherwise → pure ()


handleKey
  ∷ IO ()
  → (Xlib.Position → Xlib.Position → IO ())
  → [(Xlib.KeyCode, (Xlib.Position, Xlib.Position))]
  → Letter
  → Xlib.KeyCode
  → IO ()

handleKey done placeAt places letter keyCode
  | keyCode ≡ 9  = done -- Escape
  | keyCode ≡ 36 = resolve ∘ letterToKeyCode $ letter -- Enter
  | otherwise    = resolve keyCode
  where
    coordsByKeyCode keyCode' = snd <$> find ((≡ keyCode') ∘ fst) places
    resolve (coordsByKeyCode → Just coords) = uncurry placeAt coords >> done
    resolve _ = pure ()


data LinePointsRelativity = Absolute | Relative deriving stock (Eq, Show)
data Line = Line LinePointsRelativity [Xlib.Point] deriving stock (Eq, Show)

draw ∷ Xlib.Display → Xlib.Window → Xlib.GC → Letter → IO ()
draw dpy wnd gc (letterToLines → linesToRender) =
  forM_ linesToRender $ \(Line rel points) →
    let
      f n rescale = (round ∷ Rational → Xlib.Position) ∘ rescale ∘ fromIntegral $ n

      rescaledPoints = points <&> \(Xlib.Point x y) →
        Xlib.Point
          (f x (× ((pred w - letterPaddingX × 2) ÷ 100)))
          (f y (× ((pred h - letterPaddingY × 2) ÷ 100)))

      coordMode = case rel of
        Absolute → Xlib.coordModeOrigin
        Relative → Xlib.coordModePrevious

      shiftPadding = case rel of
        Absolute → fmap shift
        Relative → \case (x : xs) → shift x : xs; [] → []
        where shift (Xlib.Point x y) = Xlib.Point (x + letterPaddingX) (y + letterPaddingY)
    in
      Xlib.drawLines dpy wnd gc (shiftPadding rescaledPoints) coordMode

-- | Manually draw letters using simple lines.
--
-- Coordinates are like percents, in range form 0 to 100.
letterToLines ∷ Letter → [Line]
letterToLines = \case
  Q → [ Line Absolute $ let spacer = 0 in
          [ Xlib.Point roundCorner 0
          , Xlib.Point (100 - roundCorner) 0
          , Xlib.Point 100 roundCorner
          , Xlib.Point 100 (100 - roundCorner - spacer)
          , Xlib.Point (100 - roundCorner) (100 - spacer)
          , Xlib.Point roundCorner (100 - spacer)
          , Xlib.Point 0 (100 - spacer - roundCorner)
          , Xlib.Point 0 roundCorner
          , Xlib.Point roundCorner 0
          ]
      , Line Relative
          [ Xlib.Point 105 125
          , Xlib.Point (-50) (-50)
          ]
      ]
  W → [ Line Absolute
          [ Xlib.Point 0 0
          , Xlib.Point 0 100
          , Xlib.Point 50 55
          , Xlib.Point 100 100
          , Xlib.Point 100 0
          ]
      ]
  E → [ Line Absolute
          [ Xlib.Point 110 0
          , Xlib.Point 0 0
          , Xlib.Point 0 45
          , Xlib.Point 110 45
          ]
      , Line Absolute
          [ Xlib.Point 0 50
          , Xlib.Point 0 100
          , Xlib.Point 110 100
          ]
      ]
  A → [ Line Absolute
          [ Xlib.Point 0 110
          , Xlib.Point 0 roundCorner
          , Xlib.Point roundCorner 0
          , Xlib.Point (100 - roundCorner) 0
          , Xlib.Point 100 roundCorner
          , Xlib.Point 100 110
          ]
      , Line Absolute
          [ Xlib.Point 0 60
          , Xlib.Point 100 60
          ]
      ]
  S → [ Line Absolute
          [ Xlib.Point 100 roundCorner
          , Xlib.Point (100 - roundCorner) 0
          , Xlib.Point roundCorner 0
          , Xlib.Point 0 roundCorner
          , Xlib.Point 0 (48 - roundCorner)
          , Xlib.Point roundCorner 48
          , Xlib.Point (100 - roundCorner) 48
          , Xlib.Point 100 (48 + roundCorner)
          , Xlib.Point 100 (100 - roundCorner)
          , Xlib.Point (100 - roundCorner) 100
          , Xlib.Point roundCorner 100
          , Xlib.Point 0 (100 - roundCorner)
          ]
      ]
  D → [ Line Absolute $
          let roundCorner' = (round ∷ Rational → Xlib.Position) (fromIntegral roundCorner × (3/2)) in
          [ Xlib.Point 0 0
          , Xlib.Point 0 100
          , Xlib.Point (100 - roundCorner') 100
          , Xlib.Point 100 (100 - roundCorner')
          , Xlib.Point 100 roundCorner'
          , Xlib.Point (100 - roundCorner') 0
          , Xlib.Point 0 0
          ]
      ]
  Z → [ Line Absolute
          [ Xlib.Point 0 0
          , Xlib.Point 90 0
          , Xlib.Point 5 100
          , Xlib.Point 105 100
          ]
      ]
  X → [ Line Absolute
          [ Xlib.Point 0 0
          , Xlib.Point 103 103
          ]
      , Line Absolute
          [ Xlib.Point 0 100
          , Xlib.Point 103 (-3)
          ]
      ]
  C → [ Line Absolute
          [ Xlib.Point 100 (roundCorner + 8)
          , Xlib.Point 100 roundCorner
          , Xlib.Point (100 - roundCorner) 0
          , Xlib.Point roundCorner 0
          , Xlib.Point 0 roundCorner
          , Xlib.Point 0 (100 - roundCorner)
          , Xlib.Point roundCorner 100
          , Xlib.Point (100 - roundCorner) 100
          , Xlib.Point 100 (100 - roundCorner)
          , Xlib.Point 100 (100 - (roundCorner + 8))
          ]
      ]
  where roundCorner = 12


data DoneApi = DoneApi
   { doneWithIt         ∷ Either SomeException () → IO ()
   , waitBeforeItIsDone ∷ IO (Either SomeException ())
   }

mkDoneHandler ∷ IO DoneApi
mkDoneHandler = newEmptyMVar <&> \mvar → DoneApi
  { doneWithIt         = putMVar mvar
  , waitBeforeItIsDone = readMVar mvar
  }


(•) ∷ (a → b) → (b → c) → a → c
(•) = flip (∘)
infixl 9 •
{-# INLINE (•) #-}

-- This operator is provided by newer version of ‘base-unicode-symbols’.
-- This adds support for older snaphots.
(×) ∷ Num a ⇒ a → a → a
(×) = (Prelude.*)
infixl 7 ×
{-# INLINE (×) #-}
