import Matrix3D









------------------- HELPER FUNCTIONS -------------------
--scales vecter between -1 and 1
makeUnit :: (Double, Double,Double) -> (Double, Double,Double)
makeUnit vec@(x,y,z) = (x/len , y/len, z/len)
            where len = vecLength vec

-- length of vector
vecLength :: (Double, Double,Double) -> Double
vecLength (x,y,z) = sqrt $ x*x + y*y + z*z


-- subtract two vectors 
vecSub :: (Double, Double,Double) -> (Double, Double,Double) -> (Double, Double,Double)
vecSub (a,b,c) (x,y,z) = (a-x , b-y , c-z)
