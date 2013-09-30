import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import SampleGraphBindings
import PriceBar
import Control.Monad
--import Data.IORef

--TODO handle parse errors
--TODO handle command line arg errors, usage message

main = do
    (progname,[numPbs]) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]

    -- Window must exist before setting callback functions.
    createWindow "Price Chart"
    reshapeCallback $= Just reshape

    let n = read numPbs :: Int
    pbs <- getPbs n "../../data/USDCHF_day.csv"
    let pbs' = map (\pb->(fromIntegral (tick pb), open pb,high pb,low pb,close pb)) pbs
    displayCallback $= display pbs' n 0.8

    let left   = (-2)::GLdouble
    let right  =   2 ::GLdouble
    let bottom =   2 ::GLdouble
    let top    =   2 ::GLdouble
    let near   =   0 ::GLdouble
    let far    =   1 ::GLdouble
    matrixMode $= Projection
    ortho left right bottom top near far
    matrixMode $= Modelview 0

    -- Screensize is the full screen, not the viewport.
    --(Size w h) <- get screenSize
    --putStrLn ("screen size = " ++ (show w) ++ " x " ++ (show h))

    mainLoop

getPbs n path = do
    pbs <- liftM (take n) (pbsFromFile 4 path)
    return pbs
