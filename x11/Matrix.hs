module Matrix3D (
  makeIdentity,
  multMat,
  -- makeMovementSequenceMatrix,
  translate,
  scale
  -- rotateX,
  -- rotateY,
  -- rotateZ,
  -- matMultPoints,
  -- matMultPoint,
  -- xProduct,
  -- makeMovementSequenceMatrix,
  -- view
)where
import Data.List (transpose)




translate :: Num a => [[a]] -> (a,a,a) -> [[a]]
translate mat vec = translate' mat vec 0

translate' ::  Num a => [[a]] -> (a,a,a) -> Int -> [[a]]
translate' mat (x,y,z) count
              | count >= 3 = mat
              | otherwise = translate' ( update mat ((get (x,y,z) count) + val)   3 count) (x,y,z) (count + 1)
                  where val = valueAt mat 3 count


-- Scale matrix
scale :: Num a => [[a]] -> (a,a,a) -> [[a]]
scale mat vec = scale' mat vec 0

scale' :: Num a => [[a]] -> (a,a,a) -> Int -> [[a]]
scale' mat (x,y,z) count
            | count >= 3 = mat
            | otherwise = scale' (update mat ((get (x,y,z) count) * val) count count) (x,y,z) (count + 1)
                where val = valueAt mat count count

matMultPoint :: Num a => [[a]] -> (a,a,a) -> (a,a,a)
matMultPoint mat point =

rotateX :: Num a => [[a]] -> Double => [[a]]
rotateX mat rad = multMat mat $ rotateX' makeIdentity sn cs
                    where cs = cos rad
                          sn = sin rad

rotateX' :: Num a => [[a]] -> Double -> Double -> [[a]]
rotateX' mat sn cs = do
                    let a = update mat cs 1 1
                    let b = update a (-sn) 1 2
                    let c = update b sn 2 2
                    return $ update c cs 2 2


-- Does Matrix multiplication
multMat :: Num a => [[a]] -> [[a]] -> [[a]]
multMat a b =  [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

-- creates a 4 x 4 identity matrix
makeIdentity :: [[Double]]
makeIdentity = makeIdentity' (makeMatrix 4 4) 0

makeIdentity' :: [[Double]] -> Int -> [[Double]]
makeIdentity' mat pos
      | pos == length mat = mat
      | otherwise = makeIdentity' (update mat 1 pos pos) (pos + 1)







-- Helper Functions
makeMatrix :: Int -> Int -> [[Double]]
makeMatrix x y = take x (cycle[(take y (cycle [0]))])

valueAt :: Num a => [[a]] -> Int -> Int -> a
valueAt mat x y = (mat !! y) !! x

rowAt :: Num a => [[a]] -> Int -> [a]
rowAt mat y = head (drop y mat)


update :: Num a =>  [[a]] -> a -> Int -> Int -> [[a]]
update mat val x y
          | y == 0 = (updateValueAt (rowAt mat y) val x) : (tail mat)
          | y >= length mat = (init mat ) ++ [(updateValueAt (rowAt mat y) val x)]
          | otherwise = (take y mat) ++ (updateValueAt (rowAt mat y) val x) : (drop (y + 1) mat)


updateValueAt :: Num a =>  [a] -> a -> Int -> [a]
updateValueAt mat val x
          | x == 0 = val : (tail mat)
          | x >= length mat = (init mat) ++ [val]
          | otherwise = (take (x) mat) ++ (val : (drop (x+1) mat))

get :: Num a => (a,a,a) -> Int -> a
get (x,y,z) pos
      | pos == 0 = x
      | pos == 1 = y
      | pos == 2 = z
      | otherwise = error "index out of bounds "
