-----------------------------------------------------------------------------
-- Counting Sort
-- CS 300 Spring 2022 Assignment 1 Part 1
-- Deadline: Monday, 7th February 2022
-- 
-- Please press alt Z to enable Word Wrap
-- 
-- Guidelines:  
-- The assignment is based on counting sort algorithm
-- Feel free to make any helper functions where ever you need them
-- Do not show your code or make it accessible to anyone other than the course staff
-- The arguments and return types of the functions should not be changed
-----------------------------------------------------------------------------
con n [] = 0
con n (x:xs) = if (x==n) then 1 + con n xs else con n xs


-- Question 1: CountOccurance function take in 2 arguments, an integer and a list, and returns the number of times the integer exists in the list.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
leng :: (Num a1, Eq a2) => [a2] -> a1
leng []=0
leng[x]=1
leng (x:xs)=if (x:xs)==[] then 0 else if xs==[] then 1 else 1+leng(xs)
countOccurance :: Int -> [Int] -> Int
countOccurance x [] = 0
countOccurance x ys = leng xs
    where xs = [xs | xs <- ys, xs == x]

-- Question 2: The maxOf function returns the maximum integer present in the list.
maxOf :: Ord a => [a] -> a
maxOf (x:xs) = if leng(x:xs)==1 then x else if maxOf(xs)>x then maxOf(xs) else x

-- Question 3: The indexArray function takes an integer and returns a list of integers from 0 till that integer (inclusive)
indexArray :: Int -> [Int]
indexArray 0=[]
indexArray 1 = [0]
indexArray x= indexArray(x-1)++[x-1]


-- Question 4: The makeCountArray function takes in 2 lists, array1 and array2 where array1 can be a list with any numbers and array2 will specifically be an array consisting elements from 0 till maximum of array1 in ascending order. For now you can safely assume that array2 will be always correct and according to the format
-- example array1 = [0,1,2,1,1,2,3] array2 = [0,1,2,3] --> return [1,3,2,1] -- explanation: 0 exists 1 time, 1 exists 3 times and so on..
makeCountArray :: [Int] -> [Int] -> [Int]
makeCountArray list1 [] = []
makeCountArray [] [] = []
makeCountArray [] list1 = []
makeCountArray list1 (y:list2) = if (list1==[]) then [] else if  (y:list2)==[] then [] else [countOccurance y (list1)] ++ makeCountArray (list1) (list2)


-- Question 5: makePairs takes in 2 integer lists and returns a list of tuples by combining both of the arrays. 
-- Example [1,2,3,4] and [4,3,2,1] -> returns [(1,4),(2,3), (3,2), (4,1)] 
makePairs :: [Int] -> [Int] -> [] (Int,Int)
makePairs [][] = []
makePairs lis pis =[(a, pis!!(a-1))| a<-lis]

-- Question 6: CountingSort function takes in an array and uses the above functions to sort the array using counting sort technique. Feel free to use any helper functions according to your needs.
-- countingSort :: [Int] -> [Int]
getValue :: Int -> [Int] -> Int
getValue i [] = -1
getValue 0 (x:xs) = x
getValue i (x:xs) = getValue (i-1) xs


cummulativeCount :: [Int] -> [Int]
cummulativeCount [] = []
cummulativeCount [x] = [x]
cummulativeCount (x:y:xs) = x : cummulativeCount (z:xs)
                            where z = x+y


changeCummulative :: Int -> [Int] -> [Int]
changeCummulative i [] = []
changeCummulative 0 (x:xs) = (x-1):xs
changeCummulative i (x:xs) = x : changeCummulative (i-1) xs


placeOneElement :: Int -> Int -> [Int] -> [Int]
placeOneElement i e [] = []
placeOneElement i e (x:xs) = if i /= 0
                                then x : placeOneElement (i-1) e xs
                                else e : xs


setArray :: [Int] -> [Int] -> [Int] -> [Int]
setArray x [] z = x
setArray x (y:ys) z = setArray array1 ys (changeCummulative y z)
                            where array1 = placeOneElement ((getValue y z)-1) y x


getLength :: [Int] -> Int
getLength [] = 0
getLength (x:xs) = 1 + getLength xs


-- Question 6: CountingSort function takes in an array and uses the above functions to sort the array using counting sort technique. Feel free to use any helper functions according to your needs.
countingSort :: [Int] -> [Int]
countingSort [] = []
countingSort list = setArray [0..getLength(list)-1] list array
                        where array = cummulativeCount (makeCountArray list (indexArray (maxOf list)))



-----------------------------------------------------------------------------
-- The End :)
-----------------------------------------------------------------------------





