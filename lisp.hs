import Data.Char
import Data.Int
import Debug.Trace

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = trace ("n: " ++ show n) $ fib (n - 1) + fib (n - 2)



--"(12 (34 222))" -> ["(", "12","(","34","222",")",")"]

--"234 (3 4) 777" -> ("234", "(3 4) 777")

--"234","(3 4) 777"

strToTuple (' ':xs) n = (reverse n, xs)
strToTuple ('(':xs) n = (reverse n,'(': xs)
strToTuple (')':xs) n = (reverse n,')': xs)
strToTuple (x:xs) n =  strToTuple xs (x:n)


--"    (24 46)" -> ("(","24 46")

firstParentToStr ('(':xs) = ("(", xs)
firstParentToStr (')':xs) = (")", xs)
firstParentToStr (' ':xs) = firstParentToStr xs

firstParentToStr xs = strToTuple xs ""


tokenize "" = [] 

tokenize xs =   (fst temp):tokenize (snd temp) 
    where temp = firstParentToStr xs 
    
    
data List = Num Int| None | Cons List List|Str [Char]  deriving Show
data Command = EVAL List|APPLY deriving Show


--392 = 3*10^2+9*10^1+2*10^0
--357%10= 7
--357/10=35
--35%10=5
--35/10=3
--3%10 = 3
--3/10 = 0



intToStr 0 x = x
intToStr n temp = intToStr (n`div`10) ((chr (n`mod`10+48)):temp)

consToStr None = ""
consToStr (Cons x None) = (lstToStr x)
consToStr (Cons x xs) = (lstToStr x)++ " " ++ (consToStr xs)

lstToStr None = ""
lstToStr (Cons xs xy) = "("++consToStr(Cons xs xy)++")"
lstToStr (Num n) = (intToStr n "")
lstToStr (Str s) = s

--TO DO написать функцию преобразования строк в числа
--Функция проверяет, что все элементы строки - это цифры (Bool)
--добавить в  parse шаблон, что если на вход поступила строка, которая содержит число - нужно преобразровать ее в число.

--strToInt:: [a]->Int

lenTostr []  = 0
lenTostr (x:xs)  = lenTostr xs + 1



strToInt [] = 0
strToInt (x:xs) = strToInt xs + (ord x - 48)*10^lenTostr xs

checkIntInStr [] = True
checkIntInStr (x:xs) =  if (ord x - 48) >= 0 && (ord x - 48) <= 9 then checkIntInStr xs else False


checkAmountParenthesses [] [] = True
checkAmountParenthesses [] (t:ts) = False

--если встретили открывающую скобку, то клвдем ее на стек
checkAmountParenthesses ("(":xs) st  = checkAmountParenthesses xs ("(":st)

--если мы встретили закрывающую скобку, а на стеке открывающая, удалить со стека скобку и продолжить
checkAmountParenthesses (")":xs) ("(":st)  = checkAmountParenthesses xs st

--если мы встретили не скобку, то просто пропускаем и продолжаем
checkAmountParenthesses (x:xs) st  = checkAmountParenthesses xs st
--если встретил  закрывающую скобочку - скидывай из стека ВСЕ, ДО следующей открывающейся, если открывающуюся - клади на стек, если не скобка -закидывай на стек

--перебираем токены, если встречаем не закрывающую скобку, то кладем ее на стек, если встречаем закрывающую скобку,
-- то вынимаем со стека все до первой открывающей, формируем из этого новый список, и кладем на стек


takeOutUpToFirstOpenParenthesses :: List->List->List
takeOutUpToFirstOpenParenthesses (Cons  (Str "(") xs) s = Cons  s xs 
takeOutUpToFirstOpenParenthesses (Cons x xs) s = takeOutUpToFirstOpenParenthesses xs (Cons x s)
    

parse :: [[Char]]->List->List
parse [] (Cons x None) = x
parse ((checkIntInStr x):xs) stack = parse xs ((strToInt x):stack)
parse (")":xs) stack = parse xs (takeOutUpToFirstOpenParenthesses  stack None)
parse (x:xs) stack = parse xs (Cons (Str x) stack)

appendToExprWithEval :: List->[Command]
appendToExprWithEval None = []
appendToExprWithEval (Cons x xs) =  (EVAL x): appendToExprWithEval xs

eval :: [Command]->List->List
eval (EVAL (Str s):xs) stack = eval xs (Cons (Str s) stack)
eval (EVAL (Num x):xs) stack  = trace ("EVAL Num") $  eval xs (Cons (Num x) stack)
eval (EVAL (Cons x xy):xs ) stack = trace ("EVAL Cons " ++ show ((appendToExprWithEval (Cons x xy))++[APPLY]++xs)) $ eval ((appendToExprWithEval (Cons x xy))++[APPLY]++xs) stack 
eval (APPLY:y) (Cons (Str "+") (Cons (Num x) (Cons (Num s) xc))) = trace ("APPLY ") $(eval y (Cons (Num(x + s)) xc))

-- ((ф и) a)
--qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ x: qsort [y | y <- xs, y > x]  


--(+ (* 2 3) (/ 10 2))

--expr: EVAL (* 2 3)  EVAL (/ 10 2) APPLY
--stack: 12



--TODO убрать лишний пробел в функции parse

--popElementToOpenParenthesses ["("] = 

--stack:  [ "46", "42", "(" ,"(", "13"]
--stack: 13 ( ( 42 46 

--Cons 46 (Cons 42 None)

--TO DO парсер из джой перевести на русский

--12 43 45 -> "12 43 45"


--Cons (Num 42) None <- "(42)"
--Cons (Num 42) (Cons (Num 13) None) -> "(42 13)"

--To DO раскрыть рекурсию со стеком
-- "((1 5) 6)"
--"235 85 ()"
--"235", " 85 ()"

--"235"

--"35 85 ()"
