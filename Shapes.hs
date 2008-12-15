module Shapes (
               Brush
              ,Point
              ,DrawPolygon
              ,Drawing
              ,polygons)
where

import Random
import Settings
import Tools

data Color a = Color (a,a,a,a)
type ColorD = Color Double
type ColorI = Color Int

data Brush = Brush Integer Integer Integer Integer deriving (Show)
data Point = Point Integer Integer deriving (Show,Eq)
data DrawPolygon a = DrawPolygon Brush [a] deriving (Show)
data Drawing a = Drawing [a] deriving (Show)

-- | Get the polygons of a drawing
polygons :: Drawing (DrawPolygon Point) -> [DrawPolygon Point]
polygons (Drawing polygons) = polygons

instance Functor DrawPolygon where
    fmap f (DrawPolygon b pts) = DrawPolygon b (map f pts)

instance Functor Drawing where
    fmap f (Drawing ps) = Drawing (map f ps)


class Mutable a where
    mutate :: a -> IO a

instance Mutable Point where
    mutate = mutatePoint

initPoint :: IO Point
initPoint = do x <- getRandomNumber 0 maxWidth
               y <- getRandomNumber 0 maxHeight
               return (Point x y)

mutatePoint :: Point -> IO Point
mutatePoint (Point x y) = do mutateMax <- willMutate activeMovePointMaxMutationRate
                             Point x y <- if mutateMax then do x <- getRandomNumber 0 maxWidth
                                                               y <- getRandomNumber 0 maxHeight
                                                               return (Point x y) 
                                          else return (Point x y)
                             mutateMid <- willMutate activeMovePointMidMutationRate
                             Point x y <- if mutateMid then do x <- midX x
                                                               y <- midY y
                                                               return (Point x y)
                                          else return (Point x y)
                             mutateMin <- willMutate activeMovePointMinMutationRate
                             Point x y <- if mutateMin then do x <- minX x
                                                               y <- minY y
                                                               return (Point x y)
                                          else return (Point x y)
                             return (Point x y)

-- newMutatePoint :: Point -> IO Point
-- newMutatePoint p@(Point x y) = do point <- maybeMutatePoint activeMovePointMaxMutationRate
--                                            (do x <- getRandomNumber 0 maxWidth
--                                                y <- getRandomNumber 0 maxHeight
--                                                return (Point x y))
--                                            p
--                                   point <- maybeMutatePoint activeMovePointMidMutationRate
--                                            (do x <- midX x
--                                                y <- midY y
--                                                return (Point x y))
--                                            p
--                                   point <- maybeMutatePoint activeMovePointMinMutationRate
--                                            (do x <- minX x
--                                                y <- minY y
--                                                return (Point x y))
--                                            p
--                                   return point

-- mutatePoint :: Point -> IO Point
-- mutatePoint p@(Point x y) = maybeMutatePoint activeMovePointMaxMutationRate
--                             (do x <- getRandomNumber 0 maxWidth
--                                 y <- getRandomNumber 0 maxHeight
--                                 return (Point x y))
--                             p
--                             >>=
--                             maybeMutatePoint activeMovePointMidMutationRate
--                             (do x <- midX x
--                                 y <- midY y
--                                 return (Point x y))
--                             >>=
--                             maybeMutatePoint activeMovePointMinMutationRate
--                             (do x <- minX x
--                                 y <- minY y
--                                 return (Point x y))

mMutatePoint :: Point -> IO Point
mMutatePoint p = mutateMax p >>= mutateMid >>= mutateMin
    where mutateMax p@(Point x y) = maybeMutatePoint activeMovePointMaxMutationRate
                                    (do x <- getRandomNumber 0 maxWidth
                                        y <- getRandomNumber 0 maxHeight
                                        return (Point x y))
                                    p
          mutateMid p@(Point x y) = maybeMutatePoint activeMovePointMidMutationRate
                                    (do x <- midX x
                                        y <- midY y
                                        return (Point x y))
                                    p
          mutateMin p@(Point x y) = maybeMutatePoint activeMovePointMinMutationRate
                                    (do x <- minX x
                                        y <- minY y
                                        return (Point x y))
                                    p

------------------------------------------------------------------------------------

mutateMax :: Point -> IO Point
mutateMax p@(Point x y) = maybeMutatePoint activeMovePointMaxMutationRate
                          (do x <- getRandomNumber 0 maxWidth
                              y <- getRandomNumber 0 maxHeight
                              return (Point x y))
                          p

mutateMid :: Point -> IO Point
mutateMid p@(Point x y) = maybeMutatePoint activeMovePointMidMutationRate
                          (do x <- midX x
                              y <- midY y
                              return (Point x y))
                          p

mutateMin :: Point -> IO Point
mutateMin p@(Point x y) = maybeMutatePoint activeMovePointMinMutationRate
                          (do x <- minX x
                              y <- minY y
                              return (Point x y))
                          p

maybeMutatePoint :: Integer -> IO Point -> Point -> IO Point
maybeMutatePoint rate action unchanged = do mutate <- willMutate rate
                                            if mutate then action else return unchanged


randomMid = getRandomNumber (-activeMovePointRangeMid) activeMovePointRangeMid
randomMin = getRandomNumber (-activeMovePointRangeMin) activeMovePointRangeMin

midX, midY, minX, minY :: Integer -> IO Integer
midX x = mutateDim x maxWidth randomMid
midY y = mutateDim y maxHeight randomMid

minX x = mutateDim x maxWidth randomMin
minY y = mutateDim y maxHeight randomMin

mutateDim :: Integer -> Integer -> IO Integer -> IO Integer
mutateDim dim maxDim random = random >>= \random -> return (min (max 0 (dim + random)) maxDim)
                  
--instance Mutable (Color a) where
--    mutate c = return c

instance Mutable Brush where
    mutate = mutateBrush

initBrush :: IO Brush
initBrush = do r <- getRandomNumber 0 255
               g <- getRandomNumber 0 255
               b <- getRandomNumber 0 255
               a <- getRandomNumber 10 60
               return $ Brush r g b a

mutateBrush :: Brush -> IO Brush
mutateBrush (Brush r g b a) = do r <- maybeMutate r activeRedMutationRate 
                                      activeRedRangeMin activeRedRangeMax
                                 g <- maybeMutate g activeGreenMutationRate 
                                      activeGreenRangeMin activeGreenRangeMax
                                 b <- maybeMutate b activeBlueMutationRate 
                                      activeBlueRangeMin activeBlueRangeMax
                                 a <- maybeMutate a activeAlphaMutationRate 
                                      activeAlphaRangeMin activeAlphaRangeMax
                                 return (Brush r g b a)

maybeMutate :: Integer -> Integer -> Integer -> Integer -> IO Integer
maybeMutate unchanged rate min max = do mutate <- willMutate rate
                                        if mutate then (getRandomNumber min max) else 
                                            return unchanged


randomPoint :: IO Point
randomPoint = do x <- getRandomNumber 0 maxWidth
                 y <- getRandomNumber 0 maxHeight
                 return (Point x y)