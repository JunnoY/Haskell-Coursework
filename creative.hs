data Colour = White | Black deriving (Eq, Show)
data Quadtree = None | Cell Colour | Grid Quadtree Quadtree Quadtree Quadtree deriving (Eq, Show)

allBlack :: Quadtree
allBlack = Cell Black

allWhite :: Quadtree
allWhite = Cell White

clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
clockwise a b c d = Grid a b c d 

anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
anticlockwise a b c d = Grid a d c b


-- The coarsework function is defined recursively as it returns
-- allwhite for the 0th approximation
-- allwhite for allwhite
-- allblack for allblack
-- and recursively computes the nth approximation of each of its sub-quadrants for clockwise
coarsework :: Int -> Quadtree -> Quadtree
coarsework 0 _ = allWhite 
coarsework n allWhite = allWhite
coarsework n allBlack = allBlack
coarsework m (Grid a b c d) = clockwise (coarsework (m-1) a) (coarsework (m-1) b) (coarsework (m-1) c) (coarsework (m-1) d) --this recursively computes the subtrees of a large quadtree


-- ergodomestic is a function that takes a quadtree as input and return a boolean by checking whether its coarsework approximation goes wrong for all natural numbers
-- all is a function that in here it implements
-- for all n = 0,1,2,3,4..... that all n implies "not (goesWrong n)"
-- goesWrong is False when for any value n and any tree q coasework n q returns allWhite or allBlack or a Grid
-- goesWrong is true when for any value n and any tree q coasework n q returns something other than those listed above
ergodomestic :: Quadtree -> Bool
ergodomestic q  = all (\ n -> not (goesWrong n)) [0..]
    where goesWrong n = case coarsework n q of
            Cell White -> False
            Cell Black -> False
            Grid a b c d -> False
            _ -> True


-- I assume we can define any fair exercise as we want
-- In this case I define if a quadtree contains one or more white pixels and one of more black pixels then it is a fair exercise
-- If the quadtree contains only white pixels or only black pixels then it is not a fair exercise
hasBlack :: Quadtree -> Bool
hasBlack (Cell Black) = True
hasBlack (Grid a b c d) = hasBlack a || hasBlack b || hasBlack c || hasBlack d
hasBlack _ = False

hasWhite :: Quadtree -> Bool
hasWhite (Cell White) = True
hasWhite (Grid a b c d) = hasWhite a || hasWhite b || hasWhite c || hasWhite d
hasWhite _ = False

fair :: Quadtree -> Bool
fair q  = hasBlack q && hasWhite q


legalSet :: (Quadtree -> Bool)->[Quadtree] -> [Quadtree]
legalSet f [] = []
legalSet f (x:xs) = x:[v | v <- xs, f x && f v]

{-
my_solution is a function that takes a function from quadtree to bool as input and generates a quadtrees

It first generates all possible quatrees, for each quatree, we check if it is ergodomestic, if it is, then we write it to a list that
stores all legal quadtrees. Since we have infinite possible quadtrees, we apply each quadtree to the fair exercise as we are generating
quadtrees, if the quatree is applicable on the fair exercise, we can stop the process and return this quatree.

However, if there are actually no possible quatree that satisfy the fair test, as the process of generating a quatree runs infinitely, to 
terminate the process, we can use lazy evaluation and generators. This means that we can generate a potentially infinite sequence of objects 
that satisfy some initial conditions, and then use lazy evaluation to only compute as many objects as needed to satisfy the statement. 

If we can find a solution within a finite number of generated objects, then we know that the statement can be satisfied. 

However, if we have generated a large number of objects without finding a solution, it is reasonable to assume that no solution exists.

Therefore, in this case we can count the number of quatrees that we have so far generated, if the count is too big (for example 1 million),
then we will terminate the process
-}


my_solution :: (Quadtree -> Bool)-> Quadtree
my_solution f = go 0
  where go n
          | f (coarsework n q) = q
          | otherwise = coarsework (n+1) (Grid (go (n + 1)) (go (n + 1)) (go (n + 1)) (my_solution f))
          | n==1000000 = None
        q = Grid (go (0)) (go (0)) (go (0)) (go (0))
        

o = my_solution fair

