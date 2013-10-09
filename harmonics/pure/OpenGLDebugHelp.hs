module OpenGLDebugHelp where

import Graphics.Rendering.OpenGL
import Data.List(intersperse)

showMatrix s m = do
    putStrLn s
    xs0 <- getMatrixComponents RowMajor m   -- get numbers
    let xs1 = map show xs0                  -- get strings
    let xs2 = chunk 4 xs1                   -- get rows
    let xs3 = map (intersperse " ") xs2     -- insert spaces
    let xs4 = map concat xs3                -- format rows
    mapM_ putStrLn xs4

chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

