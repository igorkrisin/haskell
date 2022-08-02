import Distribution.Simple.Utils (xargs)

is_one_el lst = if null  lst then False else  null (tail lst) 
count_el lst | null lst  = 0
             | otherwise = count_el (tail lst) + 1

count_el' [] = 0
count_el' (x:xs) = count_el' xs + 1

return_last_el :: [a] -> a
return_last_el [x] =  x 
return_last_el (x:xs) =  return_last_el xs

del_last_el [x] = []
del_last_el (x:xs) = x:(del_last_el xs)

fact s = if s == 0 then 1 else s * fact(s-1)

fib s = if s == 0 then 1 else if s == 1 then 1 else fib(s - 1) + fib(s - 2)


--[x*2 | x <- [1,2,3], x>0]

qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ x: qsort [y | y <- xs, y > x]   

isSorted [x] = True 
isSorted [] = True
isSorted (x:xs) = if x <= head xs then isSorted xs  else False

conc [] ys = ys
conc (x:xs) ys =  conc xs (x:ys)


conc' xs ys = if xs == [] then ys else  conc' (tail xs) (head xs:ys) 

take' 0 xs = []
take' a (x:xs) =  x:(take' (a - 1) xs)


sum' [] = 0
sum' (x:xs) = x + sum' xs
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

maxi [x] = x
maxi (x:xs) = max x (maxi xs)

elem' y (x:xs) = if y == x then True else if xs == [] then False else elem' y xs 

data Color = White| Black 
data Piece = Empty| Pawn Color | Knight Color | King Color | Queen Color| Bishop Color | Rook Color 

board = [
        [Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black],  -- "[r][k][b]..."
        [Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White],
        [Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White]  -- "[r][k][b]..."
        ]





printPieces Empty = " "
printPieces (Pawn Black) = "\x265F"
printPieces (Knight Black) = "\x265E"
printPieces (Queen Black) = "\x265B"
printPieces (King Black) = "\x265A"
printPieces (Bishop Black) = "\x265D"
printPieces (Rook Black) = "\x265C"
printPieces (Pawn White)  = "\x2657"
printPieces (Knight White) = "\x2658"
printPieces (King White) = "\x2654"
printPieces (Queen White) = "\x2655"
printPieces (Bishop White) = "\x2657"
printPieces (Rook White) = "\x2656"


convertLstToStr [] = ""
convertLstToStr (x:xs) = "["++printPieces x++"]"++convertLstToStr xs

convertBoardToStr [] = ""
convertBoardToStr (x:xs) = convertLstToStr x++"\n"++convertBoardToStr xs --исправить доску и добавить юникод
 
join' y [] = ""
join' y (x:xs) = y++x++join' x xs



fromCP1251 = map (\chr -> case lookup chr rusTable of
                   Just x -> x
                   _ -> chr)
  where rusTable = zip ['\192'..'\255'] ['А'..'я']
-- арифметика Пеано


-- take' a xs = head xs:(take' (a - 1) (tail xs))
--bar = 42:bar

----square a = a*a

--(square (square 3)) = square 3*3 = square 9 = 81

--square 3 * square 3 = 3*3 * 3*3 = 9*9 = 81  


--2*3+1 = 6+1 = 7

--head x:xs = x
--tail x:xs = xs

--take 3 bar =  head bar:(take 2 (tail bar)) = head bar:  head (tail bar):(take 1 (tail (tail bar)) =
--head bar: head (tail bar): (head (tail (tail bar)):(take 0 (tail(tail(tail bar))) = 
--head bar: head (tail bar): (head (tail(tail(tail bar))): [] = 
--(head 42:bar): (head (tail 42:bar)): (head(tail(tail(tail 42:bar))): [] =
--42: head(bar):head(tail(tail(bar))): [] =
--42: head(42:bar): head(tail(tail(42:bar)): [] =
--42: 42:head(tail(bar)): [] =
--42: 42: head(tail(42:bar)): [] =
--42: 42: head(bar): [] = 
--42: 42: head(42:bar): [] = 
--42: 42: 42: [] =
--[42, 42, 42]

--take 3 bar = take 3 42:bar = take 3 42:42:bar = take 3 42:42:42:bar = ...


--foo x = 42

--foo (1+3) = foo 4 = 42   -- eager evaluation
--foo (1+3) = 42   --- laZy evaluation

--(reverse (reverse x))==x

--доделать функцию что бы работала 
--concRev - переворачивает первый список и добавляет ко второму
--conc - 
--if x>0==True: ...


--if x>0: return True
--else: return Fasle

--return x>0 

--rightTriangles' = [(a, b, c) | c <-[1..10], b<-[1..c], a<-[1..b], a^2 + b^2 == c^2, a+b+c == 24]


--3*4

--3+3+3+3

data Nat = Zero | Sc Nat deriving Show

--3 = Sc (Sc (Sc Zero))
--2 = Sc (Sc Zero))
--5 = Sc (Sc (Sc (Sc (Sc Zero))))

minus1::Nat->Nat
minus1 (Sc x) = x

add::Nat->Nat->Nat
add Zero y = y 
add (Sc x) y = add x (Sc y)  


convertToSc::Int->Nat
convertToSc 0 = Zero
convertToSc x = Sc(convertToSc (x - 1))

convertToNum::Nat->Int
convertToNum Zero = 0
convertToNum (Sc x) = 1 + convertToNum x 

sub::Nat->Nat->Nat
sub x Zero = x
sub (Sc x) (Sc y) = sub x y

--2+1 3*3

--       (Sc x)
--minus1 (Sc(Sc (Sc Zero))) -> Sc(Sc Zero)

--foo (x:xs) = 42:xs
--foo [1,2,3] 

data Lst = None | Cons Int Lst deriving Show

len None = 0
len (Cons x xs) = 1+len xs


is_one_el' (Cons x None) = True
is_one_el' None = False
is_one_el' (Cons x xs) = False

return_last_el' (Cons x None) = x
return_last_el' (Cons x xs) = return_last_el' xs

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y): zip' xs ys


elem'' _ [] = False 
elem'' n (x:xs) = if n == x then True else elem''  n xs
--(Cons 42 (Cons 13 (Cons 666 Empty))) == [42, 13, 666]

--переделать все функции со списками в списки Lst
compareWithHundred = compare 100
