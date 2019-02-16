-- Eileen van Heerde --
-- 11158459, evv446  --
-- CMPT 340, Asn 2   --

----------------------
-- From ASN1, Prob4 --
----------------------

-- ***Just used for testing 

averageThree:: Integer->Integer->Integer->Float
averageThree x y z = (fromIntegral (x + y + z))/3.0

averageThreeInOne:: (Integer, Integer, Integer)->Float
averageThreeInOne (x,y,z) = (fromIntegral (x + y + z))/3.0


---------------
-- PROBLEM 2 --
---------------

my_curry :: ((a, b, c)->d)->a->b->c->d
my_curry f x y z = f (x, y, z)

my_uncurry :: (a->b->c->d)->((a, b, c)->d)
my_uncurry f (x, y, z) = f x y z

---------------
-- PROBLEM 3 --
---------------

data MyFraction = MyFraction (Int, Int) deriving(Show)

-- Function to simplify fractions (may or may not use)
simplifyFract :: MyFraction->MyFraction
simplifyFract (MyFraction (num, den)) = 
    (MyFraction ((div num newDen), (div den newDen)))
    where 
        newDen = gcd num den 

-- Equality (==)		
instance Eq MyFraction where
    (MyFraction (nX, dX)) == (MyFraction (nY, dY)) = (nX*dY) == (nY*dX)
	

-- Multiply (*), Addition (+), Subtraction (-), Negation
instance Num MyFraction where
    (MyFraction (nX, dX)) * (MyFraction (nY, dY)) = 
        (simplifyFract (MyFraction ((nX*nY), (dX*dY))))
    
    (MyFraction (nX, dX)) + (MyFraction (nY, dY)) = 
        (simplifyFract (MyFraction (((nX*dY)+(nY*dX)), (dX*dY))))
    
    (MyFraction (nX, dX)) - (MyFraction (nY, dY)) = 
	    (simplifyFract (MyFraction (((nX*dY)-(nY*dX)), (dX*dY))))
    
    negate (MyFraction (nX, dX)) = (simplifyFract (MyFraction ((-nX), dX)))

    abs (MyFraction (nX, dX))  
	    | (nX < 0) = MyFraction ((-nX), dX)
	    | otherwise = MyFraction (nX, dX)

    fromInteger a = (MyFraction ((fromIntegral a), 1))

-- Divide (/), fromRational 
instance Fractional MyFraction where
    (MyFraction (nX, dX)) / (MyFraction (nY, dY)) = 
        (simplifyFract (MyFraction ((nX*dY), (dX*nY))))


-- SmallerOrEqualTo (<=), GreaterOrEqualTo (>=), SmallerThan (<), GreaterThan(>)
instance Ord MyFraction where
    (MyFraction (nX, dX)) <= (MyFraction (nY, dY)) = ((nX*dY) <= (nY*dX))
    (MyFraction (nX, dX)) >= (MyFraction (nY, dY)) = ((nX*dY) >= (nY*dX))
    (MyFraction (nX, dX)) < (MyFraction (nY, dY)) = ((nX*dY) < (nY*dX))
    (MyFraction (nX, dX)) > (MyFraction (nY, dY)) = ((nX*dY) > (nY*dX))


---------------
-- PROBLEM 4 --
---------------

-- **PartA**
shuffle :: [a]->[a]->[a]
shuffle as [] = as
shuffle [] bs = bs
shuffle (a:as) (b:bs) = a:b:(shuffle as bs) 

-- **PartB**
split :: [a]->Int->[[a]]
split [] n = []
split xs n = [(take n xs), (drop n xs)]

-- **PartC**
inshuffle :: [a]->[a]
inshuffle xs = shuffle (s !! 1) (s !! 0) where
    s = split xs ((length xs) `div` 2)

outshuffle :: [a]->[a]		
outshuffle xs = shuffle (s !! 0) (s !! 1) where
    s = split xs ((length xs) `div` 2)
    
-- **PartD**
nshuffle :: ([a]->[a])->Int->[a]->[a]
nshuffle f n xs 
    | (n == 0) = xs
	| otherwise = nshuffle f (n-1) (f xs)

-- **PartE**
howManyShuffles :: (Eq a)=>([a]->[a])->[a]->[a]->Int
howManyShuffles f l1 l2
    | (l1 == l2) = 0
    | otherwise = (howManyShuffles f (f l1) l2) + 1




