module Matrix3D (
  makeIdentity,
  multMat,
  -- makeMovementSequenceMatrix,
  translate,
  scale,
  rotateX,
  rotateY,
  rotateZ,
  matMultPoints,
  matMultPoint
  -- xProduct,
  -- makeMovementSequenceMatrix,
  -- view
)where
import Data.List (transpose)




translate :: [[Double]] -> (Double,Double,Double) -> [[Double]]
translate mat vec = translate' mat vec 0

translate' :: [[Double]] -> (Double,Double,Double) -> Int -> [[Double]]
translate' mat (x,y,z) count
              | count >= 3 = mat
              | otherwise = translate' ( update mat ((get (x,y,z) count) + val)   3 count) (x,y,z) (count + 1)
                  where val = valueAt mat 3 count


-- Scale matrix
scale :: [[Double]] -> (Double,Double,Double) -> [[Double]]
scale mat vec = scale' mat vec 0

scale' :: [[Double]] -> (Double,Double,Double) -> Int -> [[Double]]
scale' mat (x,y,z) count
            | count >= 3 = mat
            | otherwise = scale' (update mat ((get (x,y,z) count) * val) count count) (x,y,z) (count + 1)
                where val = valueAt mat count count



--Rotate around the X axis
rotateX :: [[Double]] -> Double => [[Double]]
rotateX mat rad = multMat mat $ rotateX' makeIdentity sn cs
                    where cs = cos rad
                          sn = sin rad

rotateX' :: [[Double]] -> Double -> Double -> [[Double]]
rotateX' mat sn cs = update2 cs 1 1 $ update2 (-sn) 1 2 $ update2 sn 2 1 $ update2 cs 2 2 mat

--rotate around the Y axis
rotateY :: [[Double]] -> Double => [[Double]]
rotateY mat rad = multMat mat $ rotateY' makeIdentity sn cs
                    where cs = cos rad
                          sn = sin rad

rotateY' :: [[Double]] -> Double -> Double -> [[Double]]
rotateY' mat sn cs = update2 cs 0 0 $ update2 (-sn) 2 0 $ update2 sn 0 2 $ update2 cs 2 2 mat


-- rotate around the Z axis
rotateZ :: [[Double]] -> Double => [[Double]]
rotateZ mat rad = multMat mat $ rotateZ' makeIdentity sn cs
                    where cs = cos rad
                          sn = sin rad

rotateZ' :: [[Double]] -> Double -> Double -> [[Double]]
rotateZ' mat sn cs = update2 cs 0 0 $ update2 (-sn) 1 0 $ update2 sn 0 1 $ update2 cs 1 1 mat


-- Does Matrix multiplication
multMat ::  [[Double]] -> [[Double]] -> [[Double]]
multMat a b =  [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

-- creates a 4 x 4 identity matrix
makeIdentity :: [[Double]]
makeIdentity = makeIdentity' (makeMatrix 4 4) 0

makeIdentity' ::  [[Double]] -> Int -> [[Double]]
makeIdentity' mat pos
      | pos == length mat = mat
      | otherwise = makeIdentity' (update mat 1 pos pos) (pos + 1)


-- Multiply a point by a matrix
matMultPoint :: (Double, Double, Double) -> [[Double]] -> (Double, Double, Double)
matMultPoint val@(x,y,z) mat = ((matMultPoint' val $ rowAt mat 0),(matMultPoint' val $ rowAt mat 1),(matMultPoint' val $ rowAt mat 2))

matMultPoint' :: (Double, Double, Double) -> [Double] -> Double
matMultPoint' (x,y,z) row = ((row !! 0) * x) + ((row !! 1) * y) + ((row !! 2) * z) + (row !! 3)


matMultPoints :: [(Double,Double,Double)] -> [[Double]] -> [(Double,Double,Double)]
matMultPoints [] _  = []
matMultPoints vals mat = (matMultPoint (head vals) mat) : matMultPoints (tail vals) mat


xProduct :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
xProduct (a,b,c) (x,y,z) = (((b*z) - (y*c)), ((x*c) - (a*z)), ((a*y) - (x*b)))


-- Helper Functions
makeMatrix ::  Int -> Int -> [[Double]]
makeMatrix x y = take x (cycle[(take y (cycle [0]))])

valueAt ::  [[Double]] -> Int -> Int -> Double
valueAt mat x y = (mat !! y) !! x

rowAt ::  [[Double]] -> Int -> [Double]
rowAt mat y = head (drop y mat)


update ::   [[Double]] -> Double -> Int -> Int -> [[Double]]
update mat val x y
          | y == 0 = (updateValueAt (rowAt mat y) val x) : (tail mat)
          | y >= length mat = (init mat ) ++ [(updateValueAt (rowAt mat y) val x)]
          | otherwise = (take y mat) ++ (updateValueAt (rowAt mat y) val x) : (drop (y + 1) mat)

--update function with different order of args
update2 :: Double -> Int -> Int -> [[Double]] -> [[Double]]
update2 val x y mat = update mat val x y



addDate ::  Double -> Int -> Int -> [[Double]] -> [[Double]]
addDate val x y mat = update mat (val + ( valueAt mat x y) ) x y

multDate ::  Double -> Int -> Int -> [[Double]] -> [[Double]]
multDate val x y mat = update mat (val * ( valueAt mat x y )) x y

updateValueAt ::   [Double] -> Double -> Int -> [Double]
updateValueAt mat val x
          | x == 0 = val : (tail mat)
          | x >= length mat = (init mat) ++ [val]
          | otherwise = (take (x) mat) ++ (val : (drop (x+1) mat))

get ::  (Double,Double,Double) -> Int -> Double
get (x,y,z) pos
      | pos == 0 = x
      | pos == 1 = y
      | pos == 2 = z
      | otherwise = error "index out of bounds "
