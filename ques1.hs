
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



--Q1
cmp x y = x < y
cmp_rev x y = x > y
merge [] [] cmp =[]
merge [] li cmp = li
merge li [] cmp = li
merge (x:xs) (y:ys) cmp = if cmp x y then x: merge xs (y : ys) cmp 
                else y: merge (x:xs) ys cmp
merg [] = []
merg [x] = x
merg (x:y:xs) = merg ([(merge x y cmp_rev)] ++ xs)
rev [] = []
rev (x:xs) = rev xs ++ [x]
--This is Name of Function for q1
mergeKSortedLists li = rev (merg li)

----Q2
--Tried a LOt but cant do :(
lst [x] = True
lst (x:xs) = False
move (x:xs) = if lst (x:xs) then [[x]] else [x]:move xs

handle li = move li
td [] = []
td (x : xs ) = [handle x] ++ td xs



--Q3
summ [] = 0
summ (x:xs) = x+summ xs

iszero [] = False
iszero [x] = False
iszero (x : xs ) = if (x + summ xs) == 0 then True else False


db []= []
db (x:xs) = if iszero (x:xs) then [] else x : db xs
decBack li = rev (db (rev li))

front []= []
front (x:xs) = if iszero (x:xs) then [] else x : db xs

deadlyZeroChain [] = []
deadlyZeroChain (x:xs) = if (length (decBack (x:xs))) /= length (x:xs) then (decBack (x:xs))
                            else x : deadlyZeroChain xs
-- onetoend (x:xs) = if sum x







--Q4

o li opt=  map (opt) (li) 

to [] opt =[]
to (x:xs) opt = o x opt : to xs opt

tto opt [] =[]
tto opt (x:xs) = to x opt : tto opt xs 
map3 opt li = tto opt li

--Q5 

countOccurance x [] = 0
countOccurance x ys = length xs
    where xs = [xs | xs <- ys, xs == x]

isSublist [] li2 = True
isSublist (x:xs) li2= if countOccurance x xs >= countOccurance x li2 then False else isSublist xs li2 


--Q6
t = Node (Node (Node Nil Nil 11) (Node Nil Nil 5) 2) (Node ( Node Nil Nil 8) (Node Nil Nil 7) 3) 1
count Nil = []
count (Node Nil Nil x) = [[x]]
count (Node left right x) = map(\l -> x:l) (count left ++ count right)
con [] n = []
con (x:xs) n = if ( summ (x:xs) == n) then (x:xs) else con xs n
ind [] n= []
ind (x:xs) n = if (con x n) /= [] then (con x n) : ind xs n else ind xs n
countPaths tree n = ind (count tree) n

--Q7
tree = Node (Node ( Node Nil Nil 16) (Node Nil Nil 25 ) 4) (Node (Node Nil Nil 8 ) (Node Nil Nil 11 ) 9) 6

btTraversal = \tree -> \n->
    case tree of 
        Nil -> []
        Node left right x -> btTraversal left  (n+1) ++ [n] ++ btTraversal right (n+1)
maxOf [x] = x
maxOf (x:xs) = if x > maxOf xs then x else maxOf xs

ret n li= if n > maxOf li then [] else countOccurance n li : ret (n+1) li
getNumNodes tree =  ret 1 (btTraversal tree (1))


--Q8
chk = Node ( Node Nil Nil 3) (Node ( Node ( Node Nil Nil 1) (Node Nil Nil 7) 4) ( Node ( Node Nil Nil 3) Nil 2 ) 8) 6
one [] = []
one (x:xs) = x ++ one xs
filtr [] n= []
filtr (x:xs) n= if summ x < n then filtr xs n else x: filtr xs n
select tree n = one (filtr (count tree) n)
truncateTre Nil  n t = Nil
truncateTre (Node left right x) n t = if (countOccurance x (select t n) > 0)
    then Node (truncateTre left n t) (truncateTre right n t ) x
    else Nil
truncateTree t n = truncateTre t n t 

--Q9
last_help [] chr = ['0']
last_help (x:xs) chr = if (x==chr) then xs else last_help xs chr

strL [] n= []
strL (x:xs) n = [(x,n)] ++ strL xs (n+1)

on [] li match= []
on li [] match= []
on (x:xs) li match = if ((last_help li x) /= [ '0']) then x :(on xs (last_help li x) (match+1))
                    else on xs  li match

list [] str2 = []
list (x:xs) str2 = (on (x:xs) str2 0) : list xs str2

seqq [x] = x
seqq (x:y:xs) = if (length x > length y) then x else seqq (y:xs)

longestCommonSubsequence str1 str2 = seqq (list str1 str2)
