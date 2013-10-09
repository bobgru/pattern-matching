import Graphics.UI.GLUT
import SampleGraphBindings
import SampleGraphDisplay(GLPB)
import PriceBar
import Control.Monad
import Harmonics
--import Data.IORef

--TODO handle parse errors
--TODO handle command line arg errors, usage message

myExtremaStrength = 5
myRwRk = 2.0 :: Price
myRisk = 21.0 :: Price
myRwPct = 0.50 :: Price
myTicksAfterD = 3
myMinPipsDiff = 100 :: Price


main = do
    (progname,[numPbs]) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]

    -- Window must exist before setting callback functions.
    createWindow "Price Chart"
    reshapeCallback $= Just reshape

    let n = read numPbs :: Int
    --pbs <- getPbs n "../../../data/USDCHF_hour.csv"
    let path = "../../data/USDCHF_day.csv"
    pbs <- getPbs n path
    ms <- test2 pbs
    let pbs' = map toGLPB pbs
    let ms'  = map (map toGLPB . snd) ms
    
    displayCallback $= display pbs' n 0.8 ms'

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

toGLPB :: PriceBar Price -> GLPB
toGLPB pb = (tick pb,cvt (open pb),cvt (high pb),cvt (low pb),cvt (close pb))

cvt = fromRational . toRational

getPbs :: Int -> String -> IO [PriceBar Price]
getPbs n path = liftM (take n) (pbsFromFile 4 path)

test pbs f = do
    let exts = extrema myExtremaStrength pbs
    let pn = gartley
    f pn myMinPipsDiff pbs exts

test2 pbs = test pbs $ \p d pbs xs -> do
                        let ms = allMatches [p] d xs
                        let ms' = coalesce ms
                        let ms'' = filter (acceptRwRk myRisk myRwPct myRwRk) ms'
                        return ms''
