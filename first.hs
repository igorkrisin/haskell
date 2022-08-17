import Data.ByteString (count)

maxim :: Ord a => [a] -> a
maxim [] = error"dffd"
maxim [x] = x
maxim (x:xs) = max x (maxim xs) 

replicate' 0 xs = []
replicate' n xs = xs: replicate' (n-1) xs

take' 0 xs = []
take' n [] = []
take' n (x:xs) = x:take (n-1) xs

reverse' [] = []
reverse' (x:xs) = reverse' xs++[x]

repeat' n = n:repeat' n

zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' n [] = False
elem' n (x:xs) = if x == n then True else elem' n xs
fact 0 = 1
fact 1 = 1
fact n = fact(n-1)*n


countNum 0 xs = xs
countNum n xs = countNum (n-1) (n:xs)

--countNum 3-1 (3:xs)->countNum 2-1 2:[3]->countNum 1-1 1:[2,3]->countNum 0-> [1,2,3]

countNum'' 0 xs = xs
countNum'' n xs = n:countNum'' (n-1) xs

-- 3:countNum''(3-1) [] -> 3:2:countNum''(2-1) [] -> 3:2:1:countNum''(1-1) [] -> 3:2:1:[] -> [3,2,1]


countNum' 0 0 = 0
countNum' n m = if n == m then m else countNum' (n+1) m

itSquaresTwo n = if n == 1 then True else if n > 1 && n < 2 then False else itSquaresTwo (n/2)

decrementNum 0 = 0
decrementNum n = decrementNum (n-1)

fact' 0 = 1
fact' 1 = 1
fact' n =  n * fact'(n-1)

