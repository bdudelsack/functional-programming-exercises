module Graphics where
    import Data.Char

    data Point = Point Double Double
        deriving Show

    data Object =
        Rect Point Point Style
        | Circle Point Double Style
        deriving Show

    data Color =
        Black
        | Red
        | Green
        | Blue
        deriving Show

    data Style = Style Color
        deriving Show

    colorToStr :: Color -> String
    colorToStr c= map Data.Char.toLower (show c)

    styleToAttr :: Style -> String
    styleToAttr(Style c) = "style=\"fill: " ++ colorToStr c ++ "; stroke: " ++ colorToStr c ++ ";\"";

    defaultStyle :: Style
    defaultStyle = Style Black

    type Graphic = [Object]

    single :: Object -> Graphic
    single o = [o]

    (<>) :: Graphic -> Graphic -> Graphic
    (<>) [] g = g
    (<>) g [] = g
    (<>) g (o:os) = (o : g) <> os

    objToSVG :: Object -> String
    objToSVG(Rect (Point x1 y1) (Point x2 y2) s) = "<rect x=\"" ++ show x1 ++ "\" y=\"" ++ show y1 ++ "\" width=\""
         ++ show x2 ++ "\" height=\"" ++ show y2 ++ "\" " ++ styleToAttr s ++ "/>" 
    objToSVG(Circle (Point x y) r s) = "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"" ++ show r ++ "\" " ++ styleToAttr s ++ "/>"

    toSVG :: Graphic -> String
    toSVG g = "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++ toSVG_ g ++ "\n</svg>"; 

    toSVG_ :: Graphic -> String
    toSVG_ [] = ""
    toSVG_ [o] = objToSVG o
    toSVG_ (o:g) = objToSVG o ++ "\n" ++ toSVG_ g  

    rectangle :: Double -> Double -> Graphic
    rectangle d1 d2 = single (Rect (Point 0.0 0.0) (Point d1 d2) defaultStyle)

    circle :: Double -> Graphic
    circle r = single (Circle (Point (0.0 + r) (0.0 + r)) r defaultStyle)

    colored' :: Color -> Object -> Object
    colored' c (Rect p1 p2 _) = Rect p1 p2 (Style c)
    colored' c (Circle p r _) = Circle p r (Style c)

    colored :: Color -> Graphic -> Graphic
    colored c = map (colored' c)
