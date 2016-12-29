module XML where
    import Graphics

    anchor :: XML
    anchor = XNode "a" ["href":="http://google.de"] [XText "link to ", XText "google"]

    data XML = XText String
        | XNode String [Attr] [XML]
        deriving Show

    data Attr = String := String
        deriving Show

    -- wandelt ein Attribute in die entsprechende String- Darstellung um
    attrToString :: Attr -> String
    attrToString (k := v) = "\"" ++ k ++ "\"=\"" ++ v ++ "\""

    -- wandelt eine Liste von Attributen in die entsprechende String- Darstellung um
    attrsToString :: [Attr] -> String
    attrsToString a = unwords (map attrToString a)  

    -- wandelt ein XML-Dokument in die entsprechende String- Darstellung um
    xmlToString :: XML -> String
    xmlToString (XText s) = s
    xmlToString (XNode tag attrs xml) = "<" ++ unwords [tag, attrsToString attrs] ++ ">" ++ concatMap xmlToString xml ++  "</" ++ tag ++ ">" 

    -- wandelt den Style in die String- Darstellung um
    styleToAttr' :: Style -> String
    styleToAttr' (Style c) = "fill: " ++ colorToStr c ++ "; stroke: " ++ colorToStr c ++ ";"

    -- wandelt ein Grafikobjekt in die SVG- Darstellung um
    objToSVG' :: Object -> XML
    objToSVG' (Rect (Point x1 y1) (Point x2 y2) s) = XNode "rect" ["x" := show x1, "y" := show y1, "width" := show x2, "height" := show y2, "style" := styleToAttr' s] []
    objToSVG' (Circle (Point x y) r s) = XNode "circle" ["cx" := show x, "cy" := show y, "r" := show r, "style" := styleToAttr' s] []

    -- wandelt eine Graphik in die SVG- Darstellung um
    toSVG' :: Graphic -> XML
    toSVG' g = XNode "svg" ["version" := "1.1", "xmlns" := "http://www.w3.org/2000/svg"] (map objToSVG' g) 