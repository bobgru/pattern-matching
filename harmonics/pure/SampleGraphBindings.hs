module SampleGraphBindings (idle,display,reshape,keyboardMouse) where
import Graphics.UI.GLUT
import SampleGraphDisplay
reshape s@(Size w h) = do 
    -- Fill the window.
  viewport $= (Position 0 0, s)

    -- Fill a part of the window (example).
--  viewport $= (Position (w `div` 4) (h `div` 4), Size (w `div` 2) (h `div` 2))
  postRedisplay Nothing

keyboardAct a p (Char ' ') Down = do
    a' <- get a
    a $= -a'
keyboardAct a p (Char '+') Down = do
    a' <- get a
    a $= 2*a'
keyboardAct a p (Char '-') Down = do
    a' <- get a
    a $= a'/2
keyboardAct a p (SpecialKey KeyLeft) Down = do
    (x,y) <- get p
    p $= (x-0.1,y)
keyboardAct a p (SpecialKey KeyRight) Down = do
    (x,y) <- get p
    p $= (x+0.1,y)
keyboardAct a p(SpecialKey KeyUp) Down = do
    (x,y) <- get p
    p $= (x,y+0.1)
keyboardAct a p (SpecialKey KeyDown) Down = do
    (x,y) <- get p
    p $= (x,y-0.1)
keyboardAct _ _ _ _ = return ()
keyboardMouse angle pos key state modifiers position =
    keyboardAct angle pos key state
