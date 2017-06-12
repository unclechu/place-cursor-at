#!/usr/bin/env stack
{- stack runghc
 --resolver lts-7.22
 --install-ghc
 --package base-unicode-symbols
 --package X11
 -}

{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns -fprint-potential-instances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

import Prelude.Unicode
import System.Environment (getArgs)

import Data.Bits ((.|.))
import Data.Maybe (isJust, fromJust)
import Data.List (find)

import Control.Monad (forever, forM_, filterM)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (try)
import Control.Arrow ((***))

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

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (Storable (poke))
import Foreign.C.Types (CInt)
import Foreign.C.String (castCharToCChar, peekCString)

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


data Pos = PosLT | PosCT | PosRT
         | PosLC | PosCC | PosRC
         | PosLB | PosCB | PosRB
           deriving (Eq, Show)

positions ∷ [ (Pos, (String, KeyCode)) ]
positions = [ (PosLT, ("Q", 24)), (PosCT, ("W", 25)), (PosRT, ("E", 26))
            , (PosLC, ("A", 38)), (PosCC, ("S", 39)), (PosRC, ("D", 40))
            , (PosLB, ("Z", 52)), (PosCB, ("X", 53)), (PosRB, ("C", 54))
            ]


main ∷ IO ()
main = do

  doneHandler ← getDoneHandler
  let done ∷ IO (); done = doneIt doneHandler

  dpy ← openDisplay ""
  let rootWnd = defaultRootWindow dpy

  -- Killing previous instance
  fmap (\(_, _, x) → x) (queryTree dpy rootWnd)

    >>= filterM (let f ∷ Window → IO Bool
                     f x = (try $ mf x ∷ IO (Either IOError Bool))
                           <&> \case Left _ → False; Right b → b

                     mf ∷ Window → IO Bool
                     mf x = (fmap tp_value (getTextProperty dpy x wM_CLASS) >>= peekCString)
                            <&> (≡ "place-cursor-at")

                  in f)

    >>= mapM_ (killClient dpy)

  xsn ← getArgs <&> \case [ ] → Nothing
                          [x] → Just (read (x ∷ String) ∷ CInt)
                          (_) → error "Incorrect arguments"

  (mX, mY) ← mousePos dpy rootWnd <&> (fromInteger *** fromInteger)

  xsi ← xineramaQueryScreens dpy
        <&> fromJust
        <&> \list → fromJust $ flip find list $
               case xsn of
                    Just x  → (≡ x) ∘ (+ 1) ∘ xsi_screen_number
                    Nothing → \e → let (x1, y1) = (xsi_x_org e, xsi_y_org e)
                                       (x2, y2) = (x1 + xsi_width e, y1 + xsi_height e)
                                    in mX ≥ x1 ∧ mY ≥ y1
                                     ∧ mX < x2 ∧ mY < y2

  xsi `seq` closeDisplay dpy

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

      relativeY pos
        | pos ∈ [PosLT, PosCT, PosRT] = xH ⋅ offsetPercent ÷ 100
        | pos ∈ [PosLC, PosCC, PosRC] = xH ÷ 2
        | pos ∈ [PosLB, PosCB, PosRB] = xH ⋅ (100 - offsetPercent) ÷ 100

      toPosition ∷ ℚ → Position
      toPosition = read ∘ show ∘ (round ∷ ℚ → ℤ)

      windows ∷ [(String, (Position, Position))]
      windows = flip map positions $ \(pos, (text, _)) →

        let x, y ∷ ℚ
            x = xX + relativeX pos - (w ÷ 2)
            y = xY + relativeY pos - (h ÷ 2)

         in (text, (toPosition x, toPosition y))

      places ∷ [(KeyCode, (Position, Position))]
      places = flip map positions $ \(pos, (_, keyCode)) →

        let x, y ∷ ℚ
            x = xX + relativeX pos
            y = xY + relativeY pos

         in (keyCode, (toPosition x, toPosition y))

  forM_ windows $ forkIO ∘ windowInstance done places
  waitForDone doneHandler


windowInstance ∷ IO ()
               → [(KeyCode, (Position, Position))]
               → (String, (Position, Position))
               → IO ()

windowInstance done places (text, (wndX, wndY)) = do

  dpy ← openDisplay ""

  let rootWnd = defaultRootWindow dpy
      screen  = defaultScreen     dpy
      gc      = defaultGC         dpy screen
      blackPx = blackPixel        dpy screen
      whitePx = whitePixel        dpy screen

  fontStruct ← loadQueryFont dpy fontQ
  setFont dpy gc $ fontFromFontStruct fontStruct
  setForeground dpy gc whitePx

  let placeAt ∷ Position → Position → IO ()
      placeAt = placeCursorAt dpy rootWnd

  wnd ← createSimpleWindow dpy rootWnd 0 0 w h 32 whitePx blackPx
  shPtr ← allocSH
  xSetWMNormalHints dpy wnd shPtr
  xSetWMSizeHints dpy wnd shPtr $ pMinSizeBit .|. pMaxSizeBit

  storeName dpy wnd $ "Place Cursor At [" ⧺ text ⧺ "]"
  changeProperty8 dpy wnd wM_CLASS sTRING propModeReplace $ map castCharToCChar "place-cursor-at"

  mapWindow dpy wnd
  placeWindowAt dpy wnd wndX wndY

  selectInput dpy wnd $ keyPressMask .|. exposureMask
  () <$ allocaXEvent (forever ∘ evLoop done dpy wnd gc fontStruct placeAt text places)


allocSH ∷ IO (Ptr SizeHints)
allocSH = (unsafeForeignPtrToPtr <$> malloc) >>= (\ptr → ptr <$ poke ptr sh)

  where malloc ∷ IO (ForeignPtr SizeHints)
        malloc = mallocForeignPtr

        sh ∷ SizeHints
        sh = SizeHints { sh_min_size    = Just (w, h)
                       , sh_max_size    = Just (w, h)
                       , sh_resize_inc  = Nothing
                       , sh_aspect      = Nothing
                       , sh_base_size   = Nothing
                       , sh_win_gravity = Nothing
                       }


mousePos ∷ Display → Window → IO (Integer, Integer)
mousePos dpy wnd = f <$> queryPointer dpy wnd
  where f (_, _, _, rootX, rootY, _, _, _) = (toInteger rootX, toInteger rootY)


placeWindowAt ∷ Display → Window → Position → Position → IO ()
placeWindowAt dpy wnd x y = moveResizeWindow dpy wnd x y w h

placeCursorAt ∷ Display → Window → Position → Position → IO ()
placeCursorAt dpy wnd x y = warpPointer dpy wnd wnd 0 0 0 0 x y


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

  if
   | evType ≡ keyPress → fmap getKeyCode (get_KeyEvent evPtr) >>= handleKey done placeAt places text
   | evType ≡ expose   → draw dpy wnd gc fontStruct text
   | otherwise         → return ()

  where getKeyCode (_, _, _, _, _, _, _, _, keyCode, _) = keyCode


handleKey ∷ IO ()
          → (Position → Position → IO ())
          → [(KeyCode, (Position, Position))]
          → String
          → KeyCode
          → IO ()

handleKey done placeAt places text keyCode
  | keyCode ≡ 9  = done -- Escape
  | keyCode ≡ 36 = handleKey done placeAt places (⊥) currentWindowKeyCode -- Enter
  | isJust found = placeAt (fst $ fromJust found) (snd $ fromJust found) >> done
  | otherwise    = return ()

  where found = snd <$> find ((≡ keyCode) ∘ fst) places
        currentWindowKeyCode = fromJust $ fmap (snd ∘ snd) $ find ((≡ text) ∘ fst ∘ snd) positions


draw ∷ Display → Window → GC → FontStruct → String → IO ()
draw dpy wnd gc fontStruct text = drawString dpy wnd gc textXPos textYPos text

  where tw = textWidth fontStruct text

        textXPos, textYPos ∷ Position

        textXPos = let wndCenter = fromIntegral (w ∷ ℤ) ÷ 2 ∷ Float
                       textCenter = fromIntegral tw ÷ 2 ∷ Float
                    in read $ show (round $ wndCenter - textCenter ∷ Int)

        textYPos = let wndCenter = fromIntegral (h ∷ ℤ) ÷ 2 ∷ Float
                       textCenter = fontSize ÷ 4 ∷ Float
                    in read $ show (round $ wndCenter + textCenter ∷ Int)


type DoneHandler = MVar ()

getDoneHandler ∷ IO DoneHandler
getDoneHandler = newEmptyMVar

doneIt ∷ DoneHandler → IO ()
doneIt = flip putMVar ()

waitForDone ∷ DoneHandler → IO ()
waitForDone = readMVar


(<&>) ∷ Functor f ⇒ f a → (a → b) → f b
(<&>) = flip (<$>)
infixr 5 <&>

-- vim:et:ts=2:sts=2:sw=2:cc=101:tw=100:
