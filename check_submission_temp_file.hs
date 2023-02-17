-- Ex1

-- Define a new datatype Colour which can be either White or Black (used to distinguish Childs with different colours)
data Colour = White | Black deriving (Eq, Show)

-- Define a new datatype Quadtree which can be a Child with colour or a Parent which consists of four sub-quadtrees
data Quadtree = Child Colour | Parent Quadtree Quadtree Quadtree Quadtree deriving (Eq, Show)

-- Define a function allBlack which takes an Int as input and returns a Quadtree which can be considered as a Black Child
allBlack :: Int -> Quadtree
allBlack n = Child Black

-- Define a function allWhite which takes an Int as input and returns a Quadtree which can be considered as a White Child
allWhite :: Int -> Quadtree
allWhite n = Child White

-- Define a function clockwise which takes four quadtrees as input and returns a Quadtree consists of the four quadtrees from input, arranged in clockwised order
clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
clockwise a b c d = Parent a b c d 

-- Define a function clockwise which takes four quadtrees as input and returns a Quadtree consists of the four quadtrees from input, arranged in anti-clockwised order
anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
anticlockwise a b c d = Parent a d c b

-- Ex2
-- General Idea
{-
For exercise 2, the basic idea is to use a new Datatype QuadtreeNC which is a Child with a list of colours of its neighbours, or a QuadtreeNC with 4 Childs.
Given a Quadtree as an input, we can generate a QuadtreeNC, then we can use this QuadtreeNC to recursively compute its subtrees until the subtree is a Child 
with a list of colours of its neighbours, then for each quadtree with its Childs, we can use a function to count the number of elements that are the same colour
of the quadtree, and use a function to count the number of elements that are the different colour of the quadtree. Then if the number of elements that are
the different colour is greater than the number of elements that are the same colour, we will change the colour of the quadtree.

In general, the blur function takes a Quadtree as input then it will pass the Quadtree to generateQuadtreeNC to generate a QuadtreeNC.
The function generateQuadtreeNC will form a QuadtreeNC that has all its subtrees and a list of colours of the neighbours of each of the subtree.
Then the QuadtreeNC is passed to resultingQuadtree which takes the QuadtreeNC and it uses the "change" function to determine if we need
to change the colour of the subtree and changes the colour if we need, then it will output a Quadtree version of the subtree with 
colour changed, which has no list of colours of the neighbours.

In "generateQuadtreeNC", it uses function "findNeighboursColours" and  "findNeighboursColours" uses "getParent" and "getChildsColours" to recursively compute
a QuadtreeNC with a list of colours of its neighbours.
-}

-- Define a new Quatree datatype QuadtreeNC which has a list of colours of its neighbours; QuatreeNC: quadtree with neighbour colours
-- We need this new type of Quatree because it stores the colours of all the neighbours of a quadtree in an order according to their positions
-- It allows us to retrive the colours of all the neighbours at different positions of a quadtree, 
-- then we can count the number of black neighbours and the number of white neighbours of the quadtree
-- to determine whether we change the colour of the quadtree
data QuadtreeNC = ChildNC Colour [Colour] | ParentNC QuadtreeNC QuadtreeNC QuadtreeNC QuadtreeNC deriving (Eq, Show)

-- Define a datatype Neighbour which indicates different forms of neighbour of each quadtree in different positions
-- We assume every quadtree has a neighbour at its top, bottom, left, right position, but for quatrees at the corners they might not have
-- neighbour at certain position, so the Neighbour object can be None or a NQuadtree (Neighbour Quadtree)
data Neighbour = None | NQuadtree Quadtree deriving (Eq, Show)

-- Define the name of the four subtrees of a Quatree that are also parents of their subtrees
data ParentName = A | B | C | D deriving (Eq, Show)

-- Define the name of the childs on the top, bottom, left, right of the Quadtree(Grid)
data Position = TopChilds | BottomChilds | LeftChilds | RightChilds deriving (Eq, Show)

-- Define function that gets specifc sub-quadtree A, B, C, D of a quadtree
-- For each quadtree with all its top, bottom, left, right neighbours listed,
-- getParent A, getParent B, getParent C, getParent D will accept the bottom, right, left, top neighbour of the quadtree respectively
-- if the neighbour input is a Child with colour, the function will just return the Child with colour
-- if the neighbour input is a Parent with subquadtrees a b c d, then getParent A, getParent B, getParent C, getParent D will return the subquadtree a, b, c, d respectively
-- because the other subquadtrees do not matter to the quadtree in this case

getParent :: ParentName -> Neighbour -> Neighbour
getParent _ None = None 
getParent _ (NQuadtree (Child colour)) = NQuadtree (Child colour)
getParent A (NQuadtree (Parent a b c d)) = NQuadtree a
getParent B (NQuadtree (Parent a b c d)) = NQuadtree b
getParent C (NQuadtree (Parent a b c d)) = NQuadtree c
getParent D (NQuadtree (Parent a b c d)) = NQuadtree d

-- Define a function which takes 4 neighours of a quatree as an input and form a list of neighbours
formNeighbourList :: Neighbour -> Neighbour -> Neighbour -> Neighbour -> [Neighbour]
formNeighbourList top bottom left right = [top, bottom, left, right]


-- Define a function that get a list of colours of Childs of a quadtree in specific directions
-- In a quadtree(Parent) with subquadtrees a b c d, getTopChildsColours will returns colours of subquadtrees at the top of the Parent which is a and b
getChildsColours :: Position -> Neighbour -> [Colour]
getChildsColours _ None = []
getChildsColours _ (NQuadtree (Child colour)) = [colour]
getChildsColours TopChilds (NQuadtree (Parent a b c d)) = getChildsColours TopChilds (NQuadtree a) ++ getChildsColours TopChilds (NQuadtree b)
getChildsColours BottomChilds (NQuadtree (Parent a b c d)) = getChildsColours BottomChilds (NQuadtree c) ++ getChildsColours BottomChilds (NQuadtree d)
getChildsColours LeftChilds (NQuadtree (Parent a b c d)) = getChildsColours LeftChilds (NQuadtree b) ++ getChildsColours LeftChilds (NQuadtree c)
getChildsColours RightChilds (NQuadtree (Parent a b c d)) = getChildsColours RightChilds (NQuadtree a) ++ getChildsColours RightChilds (NQuadtree d)


-- Define a function that find neighbour quadtrees with colour of a Child given certain directions
-- Given the neighbours of one side and find the Child colours on the opposite side
-- Because only Childs in one direction will have neighbour Childs in the opposite direction
findNeighboursColours :: QuadtreeNC -> [Neighbour] -> QuadtreeNC
findNeighboursColours (ChildNC colour ncolours) neighbourList = ChildNC colour (ncolours ++
    getChildsColours BottomChilds (neighbourList !! 0) ++ 
    getChildsColours TopChilds (neighbourList !! 1) ++ 
    getChildsColours RightChilds (neighbourList !! 2) ++ 
    getChildsColours LeftChilds (neighbourList !! 3))

findNeighboursColours (ParentNC a b c d) neighbourList = ParentNC 
    (findNeighboursColours a (formNeighbourList (getParent D (neighbourList !! 0)) None None (getParent B (neighbourList !! 3))))
    
    (findNeighboursColours b (formNeighbourList (getParent C (neighbourList !! 0)) None (getParent A (neighbourList !! 2)) None))
    
    (findNeighboursColours c (formNeighbourList None (getParent B (neighbourList !! 1)) (getParent D (neighbourList !! 2)) None))
    
    (findNeighboursColours d (formNeighbourList None (getParent A (neighbourList !! 1)) None (getParent C (neighbourList !! 3))))         

-- Generate QuatreeNC given a Quadtree
-- For each Quadtree with subtrees a,b,c,d, generateQuadtreeNC first turns each subtree from Quadtree type to QuadtreeNC, then each QuadtreeNC will need a list of its top, bottom, left, right
-- neighbours which we will set one of the subtrees a b c d to be the neighbour of the QuadtreeNC in each direction
generateQuadtreeNC :: Quadtree -> QuadtreeNC 
generateQuadtreeNC (Child colour) = ChildNC colour []
generateQuadtreeNC (Parent a b c d) = ParentNC 
    (findNeighboursColours (generateQuadtreeNC a) (formNeighbourList None (NQuadtree d) (NQuadtree b) None))
    (findNeighboursColours (generateQuadtreeNC b) (formNeighbourList None (NQuadtree c) None (NQuadtree a) ))
    (findNeighboursColours (generateQuadtreeNC c) (formNeighbourList (NQuadtree b) None None (NQuadtree d)))
    (findNeighboursColours (generateQuadtreeNC d) (formNeighbourList (NQuadtree a) None (NQuadtree c) None))

-- Decide whether we should change the colour of a Child

-- countSameColour takes 2 inputs, one is the colour of the current quadtree we try to determine if we should switch its colour or not
-- the second inout the a list of colours of the neighbours of the current quadtree
-- countSameColour will count the number of neighbours that have the same colour as the colour of the current quadtree
countSameColour :: Colour -> [Colour] -> Int
countSameColour colour [] = 0
countSameColour colour (x:xs) = if x == colour then 1 + countSameColour colour (xs) else countSameColour colour (xs)

-- countDifferentColour takes 2 inputs, one is the colour of the current quadtree we try to determine if we should switch its colour or not
-- the second inout the a list of colours of the neighbours of the current quadtree
-- countDifferentColour will count the number of neighbours that have different colour from the colour of the current quadtree
countDifferentColour :: Colour -> [Colour] -> Int
countDifferentColour colour [] = 0
countDifferentColour colour (x:xs) = if x /= colour then 1 + countDifferentColour colour (xs) else countDifferentColour colour (xs)

-- change takes 2 inputs, one is the colour of the current quadtree we try to determine if we should switch its colour or not
-- the second inout the a list of colours of the neighbours of the current quadtree
-- change will pass its two inputs to both countDifferentColour and countSameColour
-- if the value of countDifferentColour > the value of countSameColour, change will return true which means we should change the colour of the current quadtree
-- otherwise it returns false
change :: Colour -> [Colour] -> Bool
change colour [] = False
change colour (x:xs) = countDifferentColour colour (x:xs)  > countSameColour colour (x:xs)

-- resultingQuadtree takes a QuadtreeNC and returns a Quadtree
-- it takes a QuadtreeNC generated by generateQuadtreeNC
-- For a QuadtreeNC that is a Child with its colour and a list of neighbour colours, we pass it to the change function,
-- if change returns True, we return a Child with opposite colour (a Quadtree type)
-- if change returns False, we return a Child with same colour (a Quadtree type)
-- For a QuadtreeNC that is a Parent a b c d with its colour and a list of neighbour colours, we recursively call the resultingQuadtree function
-- with each of the subtrees as inputs until we reach the Child level

resultingQuadtree :: QuadtreeNC -> Quadtree
resultingQuadtree (ChildNC colour ncolours)
    | change colour ncolours && colour == Black = Child White 
    | change colour ncolours && colour == White = Child Black
    | otherwise = Child colour
resultingQuadtree (ParentNC a b c d) = Parent (resultingQuadtree a) (resultingQuadtree b) (resultingQuadtree c) (resultingQuadtree d) 

-- blur function
-- blur function takes a Quadtree as input then it will pass the Quadtree to generateQuadtreeNC to generate a QuadtreeNC
-- generateQuadtreeNC will form a QuadtreeNC that has all its subtrees and a list of colours of the neighbours of each of the subtree
-- Then the QuadtreeNC is passed to resultingQuadtree which takes the QuadtreeNC and it uses the "change" function to determine if we need
-- to change the colour of the subtree and changes the colour if we need, then it will output a Quadtree version of the subtree with 
-- colour changed, which has no list of colours of the neighbours
blur :: Quadtree -> Quadtree
blur (Child colour) = resultingQuadtree (generateQuadtreeNC (Child colour))
blur (Parent a b c d) = resultingQuadtree (generateQuadtreeNC (Parent a b c d))-- This file is used to test your submission
-- it checks whether your submission will work with the automated testing script
-- but it does not provide many examples.
-- More testing examples will be provided on Blackboard
-- It needs to be pasted after your submission, which is what the script check_submission.sh does
-- before compiling and running

-- testing exercise 1

t1a = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) == anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)

t1b = clockwise (allBlack 2) (anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)) (allWhite 2) (allWhite 2) == anticlockwise (allBlack 2) (allWhite 2) (allWhite 2) (clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1))

t1c = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) /= anticlockwise (allWhite 1) (allWhite 1) (allWhite 1) (allBlack 1)

mainS = putStrLn("Joe: Tests running...")
        >> putStrLn(if t1a then "Joe: Simple check for ex 1 passed!" else "ERROR: Simple check for ex1 FAILED!")
        >> putStrLn(if t1c then "Joe: Inequality check for ex 1 passed!" else "ERROR: Inequality check for ex1 FAILED!")
        >> putStrLn(if t1b then "Joe: Larger check for ex 1 passed!" else "ERROR: Larger check for ex1 FAILED!")

--

mainE = putStrLn("Joe: **************************************************************************************************")
        >> putStrLn("Joe: If any tests above FAILED your submission is not correct")
        >> putStrLn("Joe: If all tests above passed then your submission is in the correct format but...")
        >> putStrLn("Joe: ...it may not be correct: this script does NOT test thoroughly!")
--
-- This file is used to test your submission
-- it checks whether your submission will work with the automated testing script
-- but it does not provide many examples.
-- More testing examples will be provided on Blackboard
-- It needs to be pasted after your submission, which is what the script check_submission.sh does
-- before compiling and running

-- texting exercise 2

-- single cells

str1a_r = ((allWhite 1) == (blur (allWhite 1)))
str1b_r = ((allBlack 1) == (blur (allBlack 1)))

single_r = str1a_r && str1b_r

-- STRIPED

-- 4 cells

str2a_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 1)(w 1)(b 1)(b 1))
str2a_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 1)(w 1)(b 1)(b 1))
str2a_r = (str2a_o == (blur str2a_i))

-- 10 cells

str3b_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 2)(w 2)(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(w 1)(w 1)))
str3b_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(b 2)(b 2)(c(b 1)(w 1)(w 1)(w 1))(c(w 1)(b 1)(w 1)(w 1)))
str3b_r = (str3b_o == (blur str3b_i))



-- 16 cells

str4b_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(w 1)(w 1)))
str4b_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(b 1)(b 1)(w 1)(b 1))(c(b 1)(b 1)(b 1)(w 1))(c(b 1)(w 1)(w 1)(w 1))(c(w 1)(b 1)(w 1)(w 1)))
str4b_r = (str4b_o == (blur str4b_i))


--- large

str_mid_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(w 16)(w 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))))(c(w 16)(w 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(b 2)(b 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))))(w 32)(w 32))
str_mid_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(b 16)(b 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(b 1)(w 1)(b 1)(w 1))))))(c(b 16)(b 16)(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(b 1)(w 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))))(c(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(c(b 2)(b 2)(b 2)(b 2))(c(b 2)(b 2)(b 2)(b 2))(w 4)(w 4))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))(c(w 4)(w 4)(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(c(w 1)(w 1)(b 1)(b 1))))))(b 32)(b 32))
str_mid_r = (str_mid_o == (blur str_mid_i))


--- NOT STRIPED

-- 4 cells

str2b_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(w 1)(b 1)(w 1)(b 1))
str2b_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(b 1)(w 1)(b 1)(w 1))
str2b_r = (str2b_o == (blur str2b_i))

-- a medium-sized example

mid_i = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(c(b 2)(b 2)(c(b 1)(b 1)(w 1)(b 1))(b 2))(c(c(b 1)(b 1)(w 1)(b 1))(c(b 1)(b 1)(w 1)(w 1))(w 2)(w 2))(c(c(w 1)(b 1)(w 1)(b 1))(c(b 1)(w 1)(b 1)(w 1))(w 2)(w 2))(c(b 2)(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1))))(c(c(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(b 1)(w 1))(w 2)(w 2))(c(b 2)(b 2)(b 2)(c(b 1)(b 1)(b 1)(w 1)))(c(c(w 1)(w 1)(w 1)(b 1))(b 2)(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(c(w 1)(w 1)(b 1)(w 1))(c(b 1)(b 1)(w 1)(w 1))(w 2)(w 2)))(c(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(b 2)(c(b 1)(w 1)(b 1)(b 1)))(c(w 2)(b 2)(b 2)(c(w 1)(b 1)(b 1)(b 1))))(c(c(c(b 1)(w 1)(w 1)(b 1))(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(w 1)(b 1))(c(w 1)(w 1)(b 1)(w 1)))(c(c(w 1)(b 1)(b 1)(w 1))(c(b 1)(w 1)(w 1)(b 1))(b 2)(b 2))(c(c(b 1)(w 1)(b 1)(b 1))(w 2)(c(b 1)(w 1)(b 1)(b 1))(b 2))))
mid_o = let
  c = clockwise
  w = allWhite
  b = allBlack in
   (c(c(c(b 2)(b 2)(c(b 1)(b 1)(w 1)(b 1))(b 2))(c(c(b 1)(b 1)(w 1)(b 1))(c(b 1)(b 1)(w 1)(w 1))(w 2)(w 2))(c(c(w 1)(w 1)(w 1)(w 1))(c(w 1)(w 1)(w 1)(w 1))(w 2)(w 2))(c(b 2)(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1))))(c(c(c(b 1)(b 1)(w 1)(w 1))(c(b 1)(b 1)(b 1)(w 1))(b 2)(w 2))(c(b 2)(b 2)(b 2)(c(b 1)(b 1)(b 1)(w 1)))(c(c(w 1)(w 1)(w 1)(w 1))(w 2)(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(c(w 1)(w 1)(w 1)(w 1))(c(w 1)(w 1)(w 1)(w 1))(w 2)(w 2)))(c(c(w 2)(w 2)(c(w 1)(w 1)(b 1)(b 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(c(w 1)(b 1)(b 1)(w 1))(w 2))(c(w 2)(c(w 1)(b 1)(b 1)(w 1))(b 2)(c(b 1)(b 1)(b 1)(b 1)))(c(w 2)(b 2)(b 2)(c(b 1)(b 1)(b 1)(b 1))))(c(c(c(b 1)(w 1)(w 1)(b 1))(w 2)(w 2)(c(b 1)(w 1)(w 1)(b 1)))(c(w 2)(w 2)(c(w 1)(w 1)(w 1)(b 1))(c(w 1)(w 1)(b 1)(w 1)))(c(c(w 1)(b 1)(b 1)(w 1))(c(b 1)(w 1)(w 1)(b 1))(b 2)(b 2))(c(c(b 1)(w 1)(b 1)(b 1))(w 2)(c(b 1)(b 1)(b 1)(b 1))(b 2))))
mid_r = (mid_o == (blur mid_i))



main =     mainS
        >> putStrLn(if single_r  then "Joe: Single cell check for ex2 passed!" else "ERROR: Single cell check for ex2 FAILED!")
        >> putStrLn(if str2a_r then "Joe: 4 cell striped check for ex2 passed!" else "ERROR: 4 cell striped check for ex2 FAILED!")
        >> putStrLn(if str3b_r then "Joe: (fixed) 10 cell striped  check for ex2 passed!" else "ERROR: 10 cell striped check for ex2 FAILED!")
        >> putStrLn(if str4b_r then "Joe: (fixed) 16 cell striped check for ex2 passed!" else "ERROR: 16 cell striped check for ex2 FAILED!")
        >> putStrLn(if str_mid_r then "Joe: big striped check for ex2 passed!" else "ERROR: big striped check for ex2 FAILED!")
        >> putStrLn(if str2b_r then "Joe: 4 cell not striped check for ex2 passed!" else "ERROR: 4 cell not striped check for ex2 FAILED!")
        >> putStrLn(if mid_r then "Joe: medium size not striped check for ex2 passed!" else "ERROR: medium size not striped check for ex2 FAILED!")
        >> mainE
--
