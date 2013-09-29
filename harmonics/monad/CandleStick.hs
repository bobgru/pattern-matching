module CandleStick (candlestick, candlestickColor) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Draw candlestick of width w relative to (x,0,0) at low left corner.
candlestick (t,o,h,l,c) x w epsilon = do 

    let h' = max o c
    let l' = min o c
    -- Fat part of candlestick. If height of fat part is less
    -- than epsilon, move h' and l' apart until the difference
    -- is epsilon. Quads will not draw anything if rectangle is 
    -- too narrow.
    let (h'',l'') = if h' - l' > epsilon
        then (h',l')
        else (h' + (epsilon/2), l' - (epsilon/2))
    renderPrimitive Quads $ do
        vertex $ Vertex3 x l'' 0
        vertex $ Vertex3 (x+w) l'' 0
        vertex $ Vertex3 (x+w) h'' 0
        vertex $ Vertex3 x h'' 0

    let x' = x + w / 2
    renderPrimitive Lines $ do
        -- Skinny part on top
        vertex $ Vertex3 x' h' 0
        vertex $ Vertex3 x' h 0
        -- Skinny part on bottom
        vertex $ Vertex3 x' l' 0
        vertex $ Vertex3 x' l 0

candlestickColor pb = 
    color $ if bullish pb then bullishColor else bearishColor
    where bullish (_, o, _, _, c) = o < c

bullishColor = Color3 (1::GLfloat) 1 1
bearishColor = Color3 (1::GLfloat) 0 0