module Graphics where

import Data.List (intercalate)

-- Required declarations from World puzzle
-- Remove if you import the World module
data Pos = ToDoPos
data Cell = ToDoCell
data World = ToDoWorld

-- This code is based on Svg Graphics puzzle code. There is some code 
-- to get you going with generating SVG images and then problems follow

-- types for SVG
type SvgPicture = [Svg]

data Svg = Circle Integer SvgPosition String

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
        defaultBoundingBox = ((0,0), (800,600))
        

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

--------------------------------------------------------------------------------
-- Main function to test with

main = do
    writeFile "test.svg" (renderSvgPicture test1)

--------------------------------------------------------------------------------

-- (#) Generate a blue circle of radius 150 at (250,150)
-- In the default viewPort this should be in the upper left quadrant
test1 :: SvgPicture                       
test1 = undefined

-- (#) Write a function that takes an Svg shape and returns the smallest
-- bounding box that contains it. So if the box is ((x1, y1),(x2, y2)) then
-- x1 is just to the left of the Svg shape, y1 is just above, x2 is just to
-- the right and y2 is just below.
svgBoundingBox :: Svg -> SvgBox
svgBoundingBox = undefined

-- (#) Use svgBoundingBox to write a function that takes an Svg picture and
-- returns the smallest bounding box that contains all the shapes in the
-- picture.
svgPictureBoundingBox :: SvgPicture -> SvgBox
svgPictureBoundingBox = undefined

-- (#) Using svgPictureBoundingBox, replace the default viewPort bounding box 
-- in renderSvgPicture by the bounding box of the picture 

-- (#) Write translateBy that moves an Svg element by the distance specified
translateBy :: Svg -> SvgPosition -> Svg
translateBy = undefined

-- (#) Use the style "fill:none;stroke:green;stroke-width:2" to create a 
-- transparent circle then place 3 overlapping circles on picture
test2 :: SvgPicture
test2 = undefined

-- (#) Create a new example with a yellow square over the green circle in
-- test 2.
-- Tip - The height / width of the square should to 50 * sqrt 2
-- Functions to use:
-- sqrt :: Floating a => a -> a
-- round :: (Integral b, RealFrac a) => a -> b
-- floor :: (Integral b, RealFrac a) => a -> b

test3 :: SvgPicture
test3 = undefined

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
convertToTriangleUnitCoordinates = undefined

-- (#) Write scaleCoordinates. N.B. The triangle 33, 56, 65 is very close
-- to 30 degrees
scaleCoordinates :: Pos -> SvgPosition
scaleCoordinates = undefined

-- (#) Write grid that given a list of Positions it repeats the shape at 
-- each position. Test with the transparent circle from test 2
grid :: Svg -> [SvgPosition] -> [Svg]
grid = undefined

-- (#) Write test3 as a 10 * 10 hexagonal grid of transparent circles from test2
test4 :: SvgPicture
test4 = undefined

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
hexagonSvg = undefined

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
