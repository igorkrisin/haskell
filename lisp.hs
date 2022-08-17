import Data.Char

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
consToStr (Cons xs xy) = (lstToStr xs) ++ (consToStr xy)

lstToStr None = ""
lstToStr (Cons xs xy) = "("++consToStr(Cons xs xy)++")"
lstToStr (Num n) = (intToStr n "")++" "
lstToStr (Str s) = s++" "


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
parse (")":xs) stack = parse xs (takeOutUpToFirstOpenParenthesses  stack None)
parse (x:xs) stack = parse xs (Cons (Str x) stack)


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
