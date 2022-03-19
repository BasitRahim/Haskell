-----------------------------------------------------------------------------
-- CS 300 Spring 2022 Assignment 1 Part 2
-- Deadline: Monday 14th February 2022
-- 
-- Please press alt Z to enable Word Wrap
-- 
-- Guidelines:  
-- Feel free to make any helper functions where ever you need them
-- Do not show your code or make it accessible to anyone other than the course staff
-- The arguments and return types of the functions should not be changed
-- You cannot use built-in functions 
-- For queries related to the trees part of the assignment, please contact Ahmed, Sameer or Dawar.
-- For queries relared to the Matrix part of the assignment, please contact Huzaifa.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Trees
-----------------------------------------------------------------------------

data Tree a = Nil | Node (Tree a) (Tree a) a

--Following functions will allow you to print a binary tree:

helpPrint:: Int -> String
helpPrint= \arg -> 
    case arg of
        x | x==0 -> ""
        _ -> "\n" ++ concat (replicate (arg-1) "|   ") ++ "+---"
 

showTree :: (Show a) => Tree a -> Int -> String
showTree = \t -> \ind ->
    case t of
        (Node l r val) ->   
            (helpPrint ind) ++ (show val) ++ (showTree l (ind+1)) ++ (showTree r (ind+1)) 
        _ -> ""

       
instance Show a => Show (Tree a) where
    show = \t ->
        showTree t 0 



-- -- Question 1 : Represent value of Pi upto 5 dp with a binary tree and store it in the variable 'btPi'. Pi=3.14159

-- Hint:
-- Printing the tree:
-- Node (Node (Node (Node Nil (Node Nil Nil 9) 3) (Node Nil Nil 4) 6) Nil 8) (Node Nil Nil 2) 1
-- will output:
-- 1
-- +---8
-- |   +---6
-- |   |   +---3
-- |   |   |   +---9
-- |   |   +---4
-- +---2
-- btPi = Nil
btPi= Node (Node (Node Nil Nil 3) (Node Nil Nil 4) 1) (Node (Node Nil Nil 5) Nil 9) 1

-- main = print(btPi)

-- -- Part b : Represent value of Euler's number (e) upto 5 dp with a binary tree and store it in the variable 'btE'.
-- e = 2.71828

btE = Node (Node (Node Nil Nil 2) (Node Nil Nil 1) 7) (Node (Node Nil Nil 2) Nil 8) 8

-- main = print(btE)
-- lit Nil = [[]]
-- lit Nil Nil x = [[x]]
-- lit (Node left right a) = [a : lit left] ++  [a: lit right]


-- -- Question 2 : Traverse your binary tree of Pi and euler's number using Preorder, Inorder or Postorder traversal (ONLY 1 traversal) and store it in a list. Please state the traversal you will be using:
-- Traversal used: < >  

btTraversal :: Tree a -> [a]
btTraversal = \tree ->
    case tree of 
        Nil -> []
        Node left right x -> btTraversal left ++ [x] ++ btTraversal right

-- main = print(btTraversal btPi)
-- Expected output: [3,1,4,1,5,9]
-- main = print(btTraversal btE)
-- Expected output: [2,7,1,8,2,8]




-- -- Question 3:
-- For this question, you are given with 2 arguments, a binary tree and an integer n. Write a program that returns the list 
--of paths that sum to the integer n. Please note that a path has to be from the root of the tree to a leaf node.
-- Example: 

-- Value of n = 15 

-- Tree:
-- 6
-- +---4        
-- |   +---2    
-- |   |   +---1
-- |   |   +---3
-- |   +---5    
-- +---8        
-- |   +---7    
-- |   +---16 

-- output: [[6,4,2,3],[6,4,5]]
test= Node (Node (Node (Node Nil Nil 1) (Node Nil Nil 3) 2)  (Node Nil Nil 5 ) 4) (Node (Node Nil Nil 7) (Node Nil Nil 16) 8) 6
-- countPaths :: Tree Int -> Int -> [[Int]]
countPaths = \tree -> 
    case tree of 
        Nil -> []
        Node Nil Nil x
            | n - x == 0 -> [[x]]
        Node left right x 
            | otherwise ->  (x:countPaths left (n - x) ++ x:countPaths right (n - x))





-- -- Question 4:
-- For this question, you are given with 2 arguments, a binary tree and an integer n. Each node of the tree is a tuple
-- which has a unique key (which will be an integer) and a switch which can be true and false. You will pass the ball 
--from the root of the tree until it reaches a point from which it can't go forward. At a node, if its switch is off (false) 
--then the ball will go to its left, otherwise to the right. If there is no path forward then the ball stops and the next iteration 
--starts. After the ball is passed, the switch changes.. for example if a switch is off and it passes the ball to its left or right, 
--then it will turn on . You will repeat this process n times and will return the node which contains the ball at the nth iteration. 
--Find a simple visualization here: https://drive.google.com/file/d/1aGlUdtHhpKkX0nuTxfU2_5-_yF9Cj8TP/view?usp=sharing
-- Example: 

-- value of integer n = 2

-- Tree:
-- (6,True)
-- +---(5,False)    
-- |   +---(1,True) 
-- |   +---(3,False)
-- +---(2,False) 

-- Output: 1
leng :: (Num a1, Eq a2) => [a2] -> a1
leng []=0
leng[x]=1
leng (x:xs)=if (x:xs)==[] then 0 else if xs==[] then 1 else 1+leng(xs)
-- countOccurance :: Integer -> [Integer] -> Int
countOccurance x [] = 0
countOccurance x ys = leng xs
    where xs = [xs | xs <- ys, xs == x]

t=Node (Node (Node Nil Nil (1, True)) (Node Nil Nil (3, False)) (5,False)) (Node Nil Nil (2,False)) (6,True)

droppingBal :: Tree (Integer, Bool) -> Tree (Integer, Bool) ->Int -> [Integer] -> Integer
droppingBal = \tree -> \con -> \n -> \nn ->
    case tree of
        Nil -> droppingBal tree con n nn

        Node Nil Nil (x,y)
            | n==1 -> x
            | otherwise -> droppingBal con con (n-1) (x:nn)

        Node left right (x, True)
            | (countOccurance x nn) `rem` 2 ==0 ->droppingBal right con n (x:nn)
            | otherwise -> droppingBal left con  n (x:nn)
        Node left right (x, False)
            | (countOccurance x nn) `rem` 2 ==0 ->droppingBal left con n (x:nn)
            | otherwise -> droppingBal right con n (x:nn)
        




droppingBall :: Tree (Integer, Bool) -> Int -> Integer
droppingBall tree n = droppingBal tree tree n []
    

-- -- Question 5: 
-- For this Question, you will be given with a postfix expression in the form of a string. You have to construct 
--an expression tree from this string. After constructing the expression tree you will have to use it to convert it into infix.
-- Example: 
-- Postfix: a b + c d e + * *
-- infix: (a+b)*(c*(d+e))

-- Expression Tree:

-- '*'
-- +---'+'        
-- |   +---'a'    
-- |   +---'b'    
-- +---'*'        
-- |   +---'C'    
-- |   +---'+'    
-- |   |   +---'d'
-- |   |   +---'e'


constructExpressionTree :: String -> Tree String 
constructExpressionTree = undefined -- TO DO

convertToInfix :: Tree String -> String 
convertToInfix = undefined -- TO DO 




-- BONUS Question: No extra marks but the first 5 people to solve this get a treat from Ahmed and Sameer.
-- -- Question x:
-- For this question, you are given with a binary tree which may or may not be balanced. Write a program that balances the tree and returns it.  
balanceTree:: Tree Int -> Tree Int
balanceTree = undefined -- TO DO

path Nil = []
-- path Node Nil Nil a = [a]
path (Node left right a)  =  map (\l -> x:l) (path left  ++ path right)



merge [] [] cmp = []
merge [] li cmp = li
merge li [] cmp = li
merge (x:xs) (y:ys) cmp = if cmp x y then x : merge xs (y:ys) cmp else y : merge (x:xs) ys cmp
cmp x y = x < y
cmp_rev x y = x > y
-- mergeSort  li cmp = merge left right cmp
-----------------------------------------------------------------------------
-- Matrices
-----------------------------------------------------------------------------

-- -- Question 6:
-- For this task, you need to develop a function that performs any given operation element-wise on two given matrices. 
--This operation could be addition, subtraction, element-wise product or any operation that has been given to the function as an argument.
-- You cannot use any built-in functions such as map, zip, or zipWith etc. This task would be easier for you if you first make a function that performs any given operation, element-wise, on two lists e.g. 
-- thisFunction :: (a -> b -> c) -> [a] -> [b] -> [c]
-- thisFunction (+) [1, 2, 3] [4, 5, 6] returns [5,7,9]. 
-- However, there is no compulsion to make such a helper function but the same function might be helpful to use in the next part (nxn matrix multiplication) as well.
-- len :: Num a => [a] ->Int
len []=0
len[x]=1
len (x:xs)=if (x:xs)==[] then 0 else if xs==[] then 1 else 1+leng(xs)
-- thisFunction ::Num a ->(a -> b -> c) -> [a] -> [b] -> [c]
thisFunction operator (x:xs) (y:ys) = if len xs <= 0 then [(x `operator` y)]
                                        else if len ys <=0 then [(x `operator` y)]
                                        else (x `operator` y): thisFunction operator xs ys


-- matrixElemOp :: Num a => [[a]] -> [[a]] -> (a -> a -> a) -> [[a]]
matrixElemOp (x:xs) (y:ys) operator = if len xs <= 0 || len ys <= 0 then [thisFunction operator x y]
                                        else thisFunction operator x y: matrixElemOp xs ys operator

-- -- Question 7:
-- In this part, you are going to make a functional matrix product function which can multiply two matrices.
-- These matrices can be of any dimensions, so long as the number of columns of the first matrix is equal to the 
--number of rows of the second matrix (you may assume that user would not enter incompatible matrices).
-- You do not have to worry about the efficiency of your function. The following link might be useful: https://en.wikipedia.org/wiki/Matrix_multiplication#Definition. 

-- matrixMult :: Num a => [[a]] -> [[a]] -> [[a]]
-- matrixMult = undefined -- TODO
-- row [[]]=[[]]
len1 [[]] =0
len1 (x:xs)= len x


nth (x:xs) n = if n == 1 then x else nth xs (n-1)

row_n (x:xs) n = if xs == [] then [nth x n] else nth x n : row_n xs n

transpose mat n = if n == 0 then [] else transpose mat (n-1) ++ [row_n mat n]

add (x:xs) = if len xs == 0 then x else x + add xs
summ (x:xs) = add x


help list (x:xs) = if xs == [] then [add (thisFunction (*) list x) ] else  add (thisFunction (*) list x ) : help list xs

help2 (x:xs) mat2 = if xs == [] then [help x (transpose mat2 (len1 mat2))] else help x (transpose mat2 (len1 mat2)): help2 xs mat2
matrixMul mat1 mat2= transpose (help2 mat1 mat2 ) (len1 (help2 mat1 mat2) )
-- -- transpose list = first list 2



-----------------------------------------------------------------------------
-- The End :}
-----------------------------------------------------------------------------




