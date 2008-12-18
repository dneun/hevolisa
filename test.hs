module Foo where

import Char (ord)
import Random (getStdRandom,randomR)

type Magic = [Int]

jpegMagic,tiffMagic,bitmapMagic :: [Magic]
jpegMagic   = [[0xff,0xd8]]
tiffMagic   = fromString ["II","MM"]
bitmapMagic = fromString ["BM"]

fromString :: [String] -> [Magic]
fromString = (map.map) ord

hasMagic :: [Magic] -> [Int] -> Bool
hasMagic magic l = any (== l) magic

isJpeg,isTiff,isBitmap :: [Int] -> Bool
isJpeg   = hasMagic jpegMagic
isTiff   = hasMagic tiffMagic
isBitmap = hasMagic bitmapMagic

isImage :: [Int] -> Bool
--isImage l = or [isJpeg l,isTiff l, isBitmap l]
isImage l = or $ map ($l) [isJpeg,isTiff,isBitmap]



-- exercises

blockDo xs ys = do
  x <- xs
  y <- ys
  return [x,y]

guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []

multiplyTo :: Int -> [(Int,Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $
          return (x,y)

type Image = [Color]


--type Pixel = (Int,Int,Int)
--type Fitness = Int
type Color = (Double,Double,Double)
type Error = Double

-- | Measure the distance between the colors in 3D space
colorFitness :: Color -> Color -> Error
colorFitness (r1,g1,b1) (r2,g2,b2) = let deltaRed   = r1 - r2
                                         deltaGreen = g1 - g2
                                         deltaBlue  = b1 - b2
                                     in deltaRed * deltaRed +
                                        deltaBlue * deltaBlue +
                                        deltaGreen * deltaGreen

-- | Combine the pixel fitness to the total fitness ( lower is better )
error_ :: Image -> Image -> Error
error_ a b = sum $ zipWith colorFitness a b

-- | Generate a randomized color
randomColor :: IO Color
randomColor = do r <- random
                 g <- random
                 b <- random
                 return (r,g,b)
                     where random = getStdRandom (randomR (0,255))

-- | Create an image with random colors
randomImage :: Int -> IO Image
randomImage n = sequence $ replicate n randomColor
--randomImage = sequence . flip replicate randomPixel

-- | Create two random color images and compute the fitness
randomFitness :: Int -> IO Error
randomFitness n = do i1 <- randomImage n
                     i2 <- randomImage n
                     return (error_ i1 i2)


try :: Int -> IO [Error]
try n = sequence $ replicate n $ randomFitness 10

minimize :: IO [Error] -> IO [Error]
minimize = fmap (scanl1 min)

minimal :: IO [Error] -> IO Error
minimal = fmap (foldr1 min)

-- image drawing routinges

data Brush = Brush Int Int Int Int deriving (Show)
data Point = Point Integer Integer deriving (Show,Eq)
data DrawPolygon a = DrawPolygon Brush [a] deriving (Show)
data Drawing a = Drawing [a] deriving (Show)

drawing :: Drawing (DrawPolygon Point) -> IO ()
drawing = drawDrawing

selectBrush :: Brush -> IO ()
selectBrush = putStrLn . show

point :: Point -> IO ()
point = putStrLn . show

drawPolygon :: DrawPolygon Point -> IO ()
drawPolygon (DrawPolygon b ps) = selectBrush b >> mapM point ps >> return ()

drawDrawing :: Drawing (DrawPolygon Point) -> IO ()
drawDrawing (Drawing ps) = mapM drawPolygon ps >> return ()

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ fromIntegral (dx * dx + dy * dy)
    where dx = x1 - x2
          dy = y1 - y2

-- test

test = Drawing [DrawPolygon (Brush 1 2 3 4)
                            [Point 0 0,Point 0 1,Point 1 1,Point 1 4],
                DrawPolygon (Brush 5 6 7 8)
                            [Point 8 7,Point 5 4,Point 0 0,Point 3 6]]

infiniteDrawing = Drawing (repeat (DrawPolygon (Brush 1 2 3 4)
                                   (map (\x -> Point x x) [0..10])))

-- | Get the polygons of a drawing
polygons :: Drawing (DrawPolygon Point) -> [DrawPolygon Point]
polygons (Drawing polygons) = polygons

-- | Get the points of a polygon
points :: DrawPolygon Point -> [Point]
points (DrawPolygon _ p) = p

-- | Map a binary function to a list
map2 :: (a -> a -> b) -> [a] -> [b]
map2 f l = zipWith f l (tail l)

-- | Perimeter of a polygon
perimeter :: DrawPolygon Point -> Double
perimeter = sum . map2 distance . points

-- | Perimeters of all polygons
perimeters :: Drawing (DrawPolygon Point) -> [Double]
perimeters = map perimeter . polygons

-- | Sum of all perimeters of all polygons
sumPerimeters :: Drawing (DrawPolygon Point) -> Double
sumPerimeters = sum . perimeters

-- | Gaußsche Trapezformel
area :: DrawPolygon Point -> Double
area = (/2) . abs . sum . map2 term . points
    where term :: Point -> Point -> Double
          term (Point x1 y1) (Point x2 y2) = fromIntegral $ 
                                             (y1 + y2) * (x1 - x2)

closePolygon :: DrawPolygon Point -> DrawPolygon Point
closePolygon (DrawPolygon b pts) = DrawPolygon b (closePolygonPath pts)

type PolygonPath = [Point]

closePolygonPath :: PolygonPath -> PolygonPath
closePolygonPath []               = []
closePolygonPath pts | open       = pts ++ [head pts]
                     | otherwise  = pts
                     where open = head pts /= last pts

square :: Integer -> DrawPolygon Point
square n = DrawPolygon brush [Point 0 0,Point n 0,Point n n,Point 0 n,Point 0 0]
    where brush = Brush 1 2 3 4


transpose :: DrawPolygon Point -> Integer -> Integer -> DrawPolygon Point
transpose (DrawPolygon b pts) dx dy = DrawPolygon b (map transposePoint pts)
    where transposePoint (Point x y) = Point (x + dx) (y + dy)


instance Functor DrawPolygon where
    fmap f (DrawPolygon b pts) = DrawPolygon b (map f pts)

instance Functor Drawing where
    fmap f (Drawing ps) = Drawing (map f ps)