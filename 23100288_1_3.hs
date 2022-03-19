


import System.IO
alpha = ['A'..'Z']
num = ['0'..'6']
optList = [ '+', '-', '*', '/', '%', '<', '>', '!', '&' ,'|' , '[' ,']' ,'(' ,')', '{' ,'}','=']
listidentfier = ["int", "double", "float", "char","void","while","for","if","else","array","struct","class","break","case","return","cout","cin","true","false","endl","double", "float", "bool", "string"]

isfound str [] = False
isfound str (x:xs) = if x==str then True else isfound str xs

isoperator str = isfound str optList
isidentifier str = isfound str listidentfier
isdelimeter str = isfound str [";",",",":"]

helpkey [] = False
helpkey (x:xs) = if isfound x alpha then True else helpkey xs
keyword (x:xs) = if isfound x num then False else helpkey xs
getposition x "" = x
getposition x str = if take 1 str == " " then x else if take 1 str == "\n" then x-1 else getposition (x+1) (drop 1 str)

getlist []=[]
getlist str = if take 1 str == "\n"  then (take 1 str) : getlist (drop 1 str) else (take (getposition 0 str) str) : getlist (drop ((getposition 0 str) + 1) str)

take_nextline [] = []
take_nextline (x:xs) = if x == '\n' then ("\0 "++(x:xs)) else take_nextline xs
getcomments [] = []
getcomments [x] = [x]
getcomments (x:y:xs) = if x == '/' && y == '/' then getcomments (take_nextline xs) else x:getcomments (y:xs) 

take_nextlin [] = []
take_nextlin [x]="\0 "++[x]
take_nextlin (x:y:xs) = if x == '*' && y == '/' then "\0 "++xs else take_nextlin (y:xs)
getcomment [] = []
getcomment [x] = [x]
getcomment (x:y:xs) = if x == '/' && y == '*' then getcomment (take_nextlin xs) else x:getcomment (y:xs)
filte [] = []
filte (x:xs) = if (x=="") then filte xs else x:filte xs

insertspace [] = []
insertspace (y:xs) = if (isoperator y) then [' ']++[y]++[' '] ++ insertspace xs else y:insertspace xs


solve str = filte (getlist (insertspace (getcomments (getcomment str))))


coment [] = []
coment [x]=[]
coment (x:y:xs) = if x == '*' && y == '/' then [] else x : coment (y:xs)

comen [] = []
comen (x:xs) = if x =='\n' then [] else x : comen (xs)

getcom [] = []
getcom [x] = []
getcom (x:y:xs) = if x == '/' && y == '*'
    then [coment xs] ++ getcom (take_nextlin xs) 
    else if x == '/' && y == '/' then
         [comen xs] ++ getcom (take_nextline xs) 
         else getcom (y:xs)






scan [] coment= ""
scan ((x:y):xs) [] = if take 1 (x:y) == "\"" then  "<string,"++(x:y)++">  "++scan xs []
else if isfound x num then "<Num,"++(x:y)++">  "++scan xs []
else if x== '\n' then (x:y) ++ scan xs []
else if isdelimeter (x:y) then "<delimeter,"++(x:y)++">  "++scan xs []
else if isoperator x then "<operator,"++(x:y)++">  "++scan xs []
else if keyword (x:y) then "<Identifier,"++(x:y)++">  "++scan xs []
else if isidentifier (x:y) then "<keyword,"++(x:y)++">  "++scan xs []
else "<ERROR,"++(x:y)++">  "++scan xs []

scan ((x:y):xs) (a:b) = if take 1 (x:y) == "\"" then  "<string,"++(x:y)++">  "++scan xs (a:b)
else if isfound x num then "<Num,"++(x:y)++">  "++scan xs (a:b)
else if x== '\n' then (x:y) ++ scan xs (a:b)
else if isdelimeter (x:y) then "<delimeter,"++(x:y)++">  "++scan xs (a:b)
else if isoperator x then "<operator,"++(x:y)++">  "++scan xs (a:b)
else if keyword (x:y) then "<Identifier,"++(x:y)++">  "++scan xs (a:b)
else if isidentifier (x:y) then "<keyword,"++(x:y)++">  "++scan xs (a:b)
else if x=='\0' then "<comment,"++ a ++ "> "++scan xs b
else "<ERROR,"++(x:y)++">  "++scan xs (a:b)


scam str = scan (solve str) (getcom str)

main = do
    handle <- openFile "test.txt" ReadMode
    contents <- hGetContents handle
    let str = scam contents
    let file = "23100288.txt"
    fil1 <- openFile file WriteMode
    hPutStrLn fil1 str
    hClose fil1