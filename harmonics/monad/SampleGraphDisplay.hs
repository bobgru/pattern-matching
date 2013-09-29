module SampleGraphDisplay (display,idle) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import OpenGLDebugHelp
import CandleStick

-- |PriceBar information needed for graphing.
-- (open, high, low, close)
type GLPB = (GLfloat, GLfloat, GLfloat, GLfloat, GLfloat)

display :: [GLPB] -> Int -> GLfloat -> IO ()
display pbs n pbw = do 
    clear [ColorBuffer]
    --(Size ww wh) <- get windowSize
    let (ww, wh) = (2, 2)   -- default model view extents
    let hh = highestHigh pbs
    let ll = lowestLow pbs
    let vr = hh - ll
    let hr = fromIntegral n :: GLfloat
    -- xscale is adjusted to remove the extra padding that 
    -- would occur to the right of the last price bar.
    let xscale = (fromIntegral ww)/(hr - (1 - pbw))
    let yscale = (fromIntegral wh)/vr
    let xoff = -1
    let yoff = -1

    -- Compute the modelview vertical range per pixel of viewport height.
    -- Use this value as the minimum height of the fat part of the
    -- candlestick so the quad will be visible.
    (_, Size _ vph) <- get viewport
    let epsilon = vr / (fromIntegral vph) :: GLfloat

    --pm <- get (matrix (Just Projection)) :: IO (GLmatrix GLfloat)
    --showMatrix "projection matrix" pm

    -- Matrices are multiplied on the right.

    preservingMatrix $ do
        --m0 <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLfloat)
        --showMatrix "m0" m0

        -- Because matrices are multiplied by transforms on the right,
        -- this translation will be applied without scaling. This
        -- moves the origin of the display to the lower left.
        translate $ Vector3 xoff yoff (0::GLfloat)
        --m1 <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLfloat)
        --showMatrix "m1" m1

        -- Scale the part of the price chart we're showing
        -- to the part of the screen where it will be displayed.
        scale xscale yscale (1.0::GLfloat)
        --m2 <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLfloat)
        --showMatrix "m2" m2

        -- Translate the part of the price chart we're showing
        -- to the x-axis of the price chart.
        translate $ Vector3 0 ((-1) * ll) (0::GLfloat)
        --m3 <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLfloat)
        --showMatrix "m3" m3

        mapM_ (\(x, pb) -> do
            candlestickColor pb
            --showBar i x o h l c pbw
            candlestick pb x pbw epsilon
            ) (zip [0..] pbs)
    swapBuffers

highestHigh :: [GLPB] -> GLfloat
highestHigh = maximum . map (\(_,_,h,_,_) -> h)

lowestLow :: [GLPB] -> GLfloat
lowestLow   = minimum . map (\(_,_,_,l,_) -> l)

idle :: (HasSetter s, HasGetter s, HasGetter g, Num a)
     => s a -> g a -> IO ()
idle angle delta = do
  a <- get angle
  d <- get delta
  angle $=! (a+d)
  postRedisplay Nothing

showVar s v = putStrLn (s ++ " " ++ (show v))

showBar i x o h l c w = do
    let msg = (show i) ++ " " ++
              (show x) ++ " " ++
              (show o) ++ "|" ++
              (show h) ++ "|" ++
              (show l) ++ "|" ++
              (show c) ++ " " ++
              (show w)
    putStrLn msg

