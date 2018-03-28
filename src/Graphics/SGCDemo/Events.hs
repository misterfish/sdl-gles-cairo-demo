module Graphics.SGCDemo.Events ( processEvents ) where

import           Prelude hiding ( log )

import           Control.Applicative ( empty )
import           Data.Maybe ( fromJust, isJust )
import           Data.Foldable ( find )
import           Foreign ( Int32 )
import           Foreign.C ( CFloat )
import           Text.Printf ( printf )
import           Data.Monoid ( (<>) )

import           SDL.Input.Keyboard.Codes ( pattern KeycodeQ )

import           SDL as S
                 ( Event
                 , EventPayload ( KeyboardEvent, MouseMotionEvent
                                , MouseButtonEvent, MouseWheelEvent
                                , MultiGestureEvent
                                , TouchFingerEvent, TouchFingerMotionEvent )
                 , InputMotion (Pressed)
                 , Point (P)
                 , keyboardEventKeyMotion
                 , keysymKeycode
                 , keyboardEventKeysym
                 , mouseMotionEventRelMotion
                 , mouseMotionEventState
                 , mouseWheelEventPos, mouseButtonEventPos
                 , touchFingerEventPos, touchFingerMotionEventRelMotion
                 , multiGestureEventDDist
                 , eventPayload
                 , pollEvents )

import           Linear ( V2 (..)
                        , V4 (..)
                        , _x, _y )

import           Graphics.SGCDemo.Types ( Log (info, warn, err) )
import           Graphics.SGCDemo.Util ( (<|.>), allPass, inv )

doDebug = False

touchScaleAmount = 200
pinchScaleAmount = 1

processEvents log ( viewportWidth, viewportHeight )= do
    events <- pollEvents

    let debug' = debug log
        qPressed = any eventIsQPress events
        showEvents' [] = pure ()
        showEvents' x = debug' $ "polled events: " <> show x
        dragAmounts :: Maybe (Int, Int)
        dragAmounts = getEventDrag events
        wheelOrPinchAmount = getEventWheelOrPinch events
        click = getEventClick ( viewportWidth, viewportHeight ) events
        clickX = fst <$> click
        clickY = snd <$> click
        click' (Just (x, y)) = printf "[click] x: %s, y: %s" (show . fromJust $ clickX) (show . fromJust $ clickY)
    showEvents' events

    pure ( qPressed, click
         , if isJust wheelOrPinchAmount then Nothing else dragAmounts
         , wheelOrPinchAmount )

getEventClick :: (Int32, Int32) -> [Event] -> Maybe (Int32, Int32)
getEventClick ( viewportWidth, viewportHeight ) = get' . find find' . map eventPayload where
    find' (MouseButtonEvent _) = True
    find' (TouchFingerEvent _) = True
    find' _ = False
    get' (Just (MouseButtonEvent mbe)) = getMouse' ( mouseButtonEventPos mbe )
    get' (Just (TouchFingerEvent tfe)) = getTouch' ( touchFingerEventPos tfe )
    get' Nothing = empty
    getMouse' (P (V2 x y)) = pure (x, y)
    getTouch' (P (V2 x y)) = pure (scale' viewportWidth x, scale' viewportHeight y)
    scale' vp' = floor . (* frint vp')

-- looping too many times through events ... these could all be combined.
getEventWheel :: [Event] -> Maybe Int
getEventWheel = get' . find find' . map eventPayload where
    find' (MouseWheelEvent mmevd) = True
    find' _ = False
    get' (Just (MouseWheelEvent mmevd)) = get'' ( mouseWheelEventPos mmevd )
    get' Nothing = empty
    -- { -1, 0, 1 }
    get'' (V2 _ y) = pure . frint $ y

getEventPinch :: [Event] -> Maybe Int
getEventPinch = get' . find find' . map eventPayload where
    find' (MultiGestureEvent mgevd) = True
    find' _ = False
    get' (Just (MultiGestureEvent mgevd)) = get'' ( multiGestureEventDDist mgevd )
    get' Nothing = empty
    get'' = pure . normTouch' -- CFloat
    normTouch' = clamp' . floor . (/ pinchScaleAmount)
    clamp' x
      | x < 0  = min x (inv 1)
      | x >= 0 = max x 1

getEventWheelOrPinch :: [Event] -> Maybe Int
getEventWheelOrPinch = getEventWheel <|.> getEventPinch

getEventDrag :: [Event] -> Maybe (Int, Int)
getEventDrag = get' . find find' . map eventPayload where
    find' (MouseMotionEvent mmevd) = True
    find' (TouchFingerMotionEvent mmevd) = True
    find' _ = False
    get' (Just (MouseMotionEvent mmevd)) = getMouse' ( mouseMotionEventRelMotion mmevd
                                                     , mouseMotionEventState mmevd )
    get' (Just (TouchFingerMotionEvent tfmevd)) = getTouch' ( touchFingerMotionEventRelMotion tfmevd )
    get' Nothing = empty
    -- | no buttons
    getMouse' (_, []) = empty
    -- | any button held down
    -- Int32
    getMouse' (V2 x y, _)   = pure ( fromIntegral x, fromIntegral y )
    -- CFloat, [-1, 1]
    getTouch' (V2 x y)      = pure ( normTouch'' x, normTouch'' y )
    normTouch' = floor . (* touchScaleAmount)
    normTouch'' = normTouch'

eventIsQPress = event' . eventPayload where
    event' (KeyboardEvent kev) = allPass [pressed', q'] kev
    event' _                   = False
    pressed'                   = (== Pressed) . keyboardEventKeyMotion
    q'                         = (== KeycodeQ) . keysymKeycode . keyboardEventKeysym


frint :: (Num b, Integral a) => a -> b
frint = fromIntegral

debug | doDebug == True = info
      | otherwise       = const . const . pure $ ()

