module Graphics where

import Data.List (intercalate)

-- Required declarations from World puzzle
-- Remove if you import the World module
type Pos = (Integer, Integer)
data Cell = ToDoCell
data World = ToDoWorld

-- This code is based on Svg Graphics puzzle code. There is some code 
-- to get you going with generating SVG images and then problems follow

-- types for SVG
type SvgPicture = [Svg]

data Svg = Circle Integer SvgPosition String
         | Rectangle Integer Integer SvgPosition Style
         | Polygon [SvgPosition] Style

type SvgPosition = (Integer, Integer)
type SvgBox = (SvgPosition, SvgPosition)
type Style = String
         
-- types for XML tags
type TagName = String
type AttributeList = [(String, String)]
type InnerContents = String

renderTag :: TagName -> AttributeList -> InnerContents -> String
renderTag t a i = if i == ""
                    then "<" ++ t ++ " " ++ (renderAttributes a)
                            ++ "/>\n" -- No contents
                    else "<" ++ t ++ " " ++ (renderAttributes a) ++ ">\n"
                            ++ i ++ "</" ++ t ++ ">\n" -- Including contents
    where
        renderAttributes :: AttributeList -> String
        renderAttributes m = foldr (\(n, v) acc ->  n ++ "=" ++
                                            "\"" ++ v ++ "\"" ++ " " ++ acc) "" m

renderSvgPicture :: SvgPicture -> String
renderSvgPicture ss       = renderTag "svg" attrList innerContents
    where
        attrList = [ ("xmlns", "http://www.w3.org/2000/svg")
                   , ("viewBox", viewBox defaultBoundingBox)
                   , ("preserveAspectRatio", "none")
                   , ("style", "background:lightblue")
                   ]
        innerContents = (concatMap renderSvg ss)
        viewBox ((x1,y1),(x2,y2)) = intercalate "," [show x1, show y1, 
                                                show (x2-x1), show (y2-y1)] 
        defaultBoundingBox = svgPictureBoundingBox ss
      

-- Example SVG:
-- <circle style="fill:blue" r="50" cy="100" cx="100"/>
renderSvg :: Svg -> String
renderSvg (Circle r (x,y) style) = renderTag "circle"
                                        [("cx", show x)
                                              ,("cy", show y)
                                              ,("r", show r)
                                              ,("style", style)
                                              ]
                                         ""
renderSvg (Rectangle h w (x,y) style) = renderTag "rect"
                                        [("x", show x)
                                              ,("y", show y)
                                              ,("width", show w)
                                              ,("height", show h)
                                              ,("style", style)
                                              ]
                                         ""
renderSvg (Polygon ps style) = renderTag "polygon"
                                        [("points", pointsString ps)
                                              ,("style", style)
                                              ]
                                         ""
            where pointsString []     = ""
                  pointsString ((x,y):xys) = (show x) ++ "," ++ (show y) ++ " "
                                                ++ (pointsString xys)

--------------------------------------------------------------------------------
-- Main function to test with

main = do
    writeFile "test1.svg" (renderSvgPicture test1)
    writeFile "test2.svg" (renderSvgPicture test2)
    writeFile "test3.svg" (renderSvgPicture test3)
    writeFile "test4.svg" (renderSvgPicture test4)
    writeFile "test7.svg" (renderSvgPicture test7)
    
--------------------------------------------------------------------------------
-- (#) Write circle that creates a circle of specified radius at (0,0)
circle :: Integer -> Svg
circle r = Circle r (50,50) ""

-- (#) Write centreAt that creates a Svg element centred at the position
centreAt :: Svg -> SvgPosition -> Svg
centreAt (Circle r _ s) p = Circle r p s
centreAt (Rectangle h w _ s) (x, y) =
                Rectangle h w ((x - (w `div` 2)), (y - (h `div` 2))) s
centreAt (Polygon ps s) (x, y) =
                Polygon (map (\(a,b) -> (a+x,y+b)) ps) s

-- (#) Write withStyle
withStyle :: Svg -> Style -> Svg
withStyle (Circle r p _) s = Circle r p s
withStyle (Rectangle h w p _) s = Rectangle h w p s

-- (#) Add Rectangle Constructor with x coordinate, y coordinate, height, 
-- width and style. Modify the following functions:
--     renderSvg, centreAt and withStyle

-- (#) Write square that creates a square of specified height / width at (0,0)
square :: Integer -> Svg
square d = Rectangle d d (d `div` (-2), d `div` (-2)) ""
  
-- (#) Generate a blue circle of radius 150 at (250,150)
-- In the default viewPort this should be in the upper left quadrant
test1 :: SvgPicture                                         
test1 = [Circle 50 (50,50) "fill:blue"] -- A blue circle of radius 50 at (50,50)

-- (#) Write a function that takes an Svg shape and returns the smallest
-- bounding box that contains it. So if the box is ((x1, y1),(x2, y2)) then
-- x1 is just to the left of the Svg shape, y1 is just above, x2 is just to
-- the right and y2 is just below.
svgBoundingBox :: Svg -> SvgBox
svgBoundingBox (Circle r (x,y) s)       = ( (x - r, y - r), (x + r, y + r))
svgBoundingBox (Rectangle h w (x, y) s) = ( (x, y), (x + w, y + h) )
svgBoundingBox (Polygon ps s)           = ( ( minx, miny ), (maxx, maxy) )
                where
                    minx = minimum (map fst ps)
                    miny = minimum (map snd ps)
                    maxx = maximum (map fst ps)
                    maxy = maximum (map snd ps)

-- (#) Use svgBoundingBox to write a function that takes an Svg picture and
-- returns the smallest bounding box that contains all the shapes in the
-- picture.
svgPictureBoundingBox :: SvgPicture -> SvgBox
svgPictureBoundingBox ps = ( ( minx, miny ), (maxx, maxy) )
                where
                    minx = minimum (map (fst . fst . svgBoundingBox) ps)
                    miny = minimum (map (snd . fst . svgBoundingBox) ps)
                    maxx = maximum (map (fst . snd . svgBoundingBox) ps)
                    maxy = maximum (map (snd . snd . svgBoundingBox) ps)

-- (#) Using svgPictureBoundingBox, replace the default viewPort bounding box 
-- in renderSvgPicture by the bounding box of the picture 

-- (#) Write translateBy that moves an Svg element by the distance specified
translateBy :: Svg -> SvgPosition -> Svg
translateBy = undefined

-- (#) Use the style "fill:none;stroke:green;stroke-width:2" to create a 
-- transparent circle then place 3 overlapping circles on picture
test2 :: SvgPicture
test2 = map (centreAt c) [ (100,100), (150,100), (200,100) ]
    where c = circle 50 `withStyle` "fill:none;stroke:green;stroke-width:2"

-- (#) Create a new example with a yellow square over the green circle in
-- test 2.
-- Tip - The height / width of the square should to 50 * sqrt 2
-- Functions to use:
-- sqrt :: Floating a => a -> a
-- round :: (Integral b, RealFrac a) => a -> b
-- floor :: (Integral b, RealFrac a) => a -> b

test3 :: SvgPicture
test3 = test2 ++ [square (round (50 * sqrt 2))
                    `centreAt` (100,100)
                    `withStyle` "fill:yellow"]


-- (#) To build the hexagonal grid we need to translate the cell positions to
-- svg positions. With the Svg origin in the top left corner there is a small
-- triangle between the origin and the closest two verticies that can be taken 
-- as the unit X and Y distancts. The centres of hexagons are a whole multiples
-- of X and Y so the transalation from cell coordinates to picture coordinates
-- can be done in two steps. First translate into multiples of X and Y and then
-- scale by X and Y. If this make no sense have a look at the image in the puzzles
-- directory (Hexagons with Unit Triangle Coordinates.png). With pencil and paper
-- work out the coordinates required.

-- (#) Write convertToTriangleUnitCoordinates
-- N.B. Top left in source cordinates is (0,0), x increases to the right
-- and y increases going down the page
convertToTriangleUnitCoordinates :: Pos -> Pos
convertToTriangleUnitCoordinates (x, y) = (x * 2 + 1 + hexOffset, y * 3 + 2)
                            where hexOffset = if y `mod` 2 == 0 then 0 else 1

-- (#) Write scaleCoordinates. N.B. The triangle 33, 56, 65 is very close
-- to 30 degrees
scaleCoordinates :: Pos -> SvgPosition
scaleCoordinates (x,y) = (x * 56, y * 33)

-- (#) Write grid that given a list of Positions it repeats the shape at 
-- each position. Test with the transparent circle from test 2
grid :: Svg -> [SvgPosition] -> [Svg]
grid s [] = []
grid s (p:ps) = s `centreAt` p : grid s ps

-- (#) Write test3 as a 10 * 10 hexagonal grid of transparent circles from test2
test4 :: SvgPicture
test4 = grid shape hexGridCoords
    where
        hexGridCoords = map (scaleCoordinates . convertToTriangleUnitCoordinates) ps
        ps =  [ (x,y) | y <- [0..9], x <- [0..9] ]
        shape = circle 56 `withStyle` "fill:none;stroke:green;stroke-width:2"

-- (#) Add a polygon constructor to Svg to generate the polygon tag. The
-- points attribute lists the points of the polygon (with the last point 
-- being automatically linked to the first. Each point is listed two numbers 
-- seperated by a comma and the points are seperated by spaces in the attribute
-- string. The style attribute is the same as for other shapes. For example
-- the following would make a heaxagon:
--     <polygon points="0,-66 56,-33 56,33 56,33 0,66 -56,33 -56,-33"
--              style="fill:blue" />
--


-- (#) Update renderSvg for the new Polygon constructor

-- (#) Write hexagonSvg function that creates hexagon with given style centred
-- at the given position
hexagonSvg :: Style -> SvgPosition -> Svg
hexagonSvg s p = Polygon [ (0, -66), ( 56, -33), ( 56, 33), (0, 66), (-56, 33), (-56, -33)  ] s
                    `centreAt` p

hexCoords maxX maxY = map convertToTriangleUnitCoordinates
                            [ (x,y) | y <- [0..maxX - 1], x <- [0..maxY - 1] ]

test7 = map (hexagonSvg "fill:none;stroke:black;stroke-width:2")
                    (map scaleCoordinates $ hexCoords 100 100)
          
                    
-- (#) Write antSvg function that creates a triangle with given style centred
-- at the given position
antSvg :: Style -> SvgPosition -> Svg
antSvg = undefined

-- (#) Write cellSvg function that returns a Svg list for a given Cell
cellSvg :: Cell -> [Svg]
cellSvg = undefined

-- (#) Write worldSvg function that returns a SvgPicture for a given Cell
worldSvg :: World -> SvgPicture
worldSvg = undefined

-- Further exercises:
-- (#) The SVGs that have been generated contain a lot of repeated information,
-- rewrite the SVG generation to replace the repeated information with <defs>
-- and <use> tags.
