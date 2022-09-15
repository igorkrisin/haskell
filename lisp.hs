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
    
    
data List = Num Int| None | Cons List List|Str [Char]deriving Show
data Command = EVAL List|APPLY2|APPLY1|CONS|PUSH_NONE|SWITCH|APPLY_LAMBDA deriving Show


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

lenghtList None = 0
lenghtList (Cons x xs) = lenghtList xs + 1

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
parse (")":xs) stack = parse xs (takeOutUpToFirstOpenParenthesses  stack None)
parse (x:xs) stack = if (checkIntInStr x) then parse xs (Cons (Num (strToInt x)) stack) else parse xs (Cons (Str x) stack)
--parse (x:xs) stack = parse xs (Cons (Str x) stack)

appendToExprWithEval :: List->[Command]
appendToExprWithEval None = []
appendToExprWithEval (Cons x xs) =  (EVAL x): appendToExprWithEval xs

callEval [] stack alist = return()
callEval expr stack alist = printStr(show temp++"\n")>>callEval(fst temp) (snd temp) alist
    where temp = step expr stack alist
    

doubleElinFunc x 0 = []
doubleElinFunc x n = x:doubleElinFunc x (n-1)
 
assoc :: [Char]->[([Char], List)]->List
assoc keyFind ((key,value):xs) = if keyFind == key then value else assoc keyFind xs
 
assocBool :: [Char]->[([Char], List)]->Bool
assocBool keyFind ((key,value):xs) = if keyFind == key then True else assocBool keyFind xs
assocBool keyFind [] = False 

--pairlis ["a","b","c"] [1,2,3] -> [("a",1),("b",2),("c",3)]--все списки Cons

pairList None None = None
pairList (Cons (Str x) xs) (Cons (Num y)ys) =  (Cons (Str x)(Cons (Num y)(pairList xs ys)))

step :: [Command]->List->[([Char],List)]->([Command], List) 
step [] stack alist = ([], stack)
step (PUSH_NONE:y) stack alist =  (y, (Cons None stack))
step (EVAL (Cons (Cons (Str "lambda")(Cons xs ys)) var):endingExpr) stack alist =(((appendToExprWithEval xs)++[PUSH_NONE]++(doubleElinFunc CONS (lenghtList xs))++endingExpr), ((Cons xs)(Cons  var stack)))  
step (EVAL (Str s):xs) stack alist = (xs, (if assocBool s alist then  (Cons (assoc s alist) stack) else Cons (Str s) stack))
step (EVAL (Num x):xs) stack  alist=  ( xs, (Cons (Num x) stack))
step (EVAL (Cons (Str "list") xs):y) stack alist = (((appendToExprWithEval xs)++[PUSH_NONE]++(doubleElinFunc CONS (lenghtList xs))++y),stack)
step (EVAL (Cons (Str "if")( Cons cond (Cons branch1 (Cons branch2 None)))):expr) stack alist= ((EVAL cond:SWITCH:expr) , ((Cons branch1(Cons branch2 stack))))
step (SWITCH:expr)  (Cons (Str "true")(Cons branch1(Cons branch2 endingStack))) alist= ((EVAL branch1):expr, endingStack)
step (SWITCH:expr)  (Cons (Str "false")(Cons branch1(Cons branch2 endingStack))) alist = ((EVAL branch2):expr, endingStack)
step (EVAL (Cons(Str string)(Cons xs None)):expr) stack alist= (((EVAL (Str string)):(EVAL xs):APPLY1:expr), stack)--описаны сразу 2 шаблона для null and zerop
step (EVAL (Cons x xy):xs ) stack alist = (((appendToExprWithEval (Cons x xy))++[APPLY2]++xs), stack)
--step (APPLY_LAMBDA:expr) ((Cons (Cons xs ys))(Cons var endingStack)) alist = (expr, (Cons  
step (APPLY1:expr) (Cons None (Cons (Str "null") endingStack)) alist = (expr, (Cons (Str "true") endingStack)) 
step (APPLY1:expr) (Cons xs (Cons (Str "null") endingStack)) alist = (expr, (Cons (Str "false") endingStack))
step (APPLY1:expr) (Cons (Num 0) (Cons (Str "zerop") endingStack)) alist = (expr, (Cons (Str "true") endingStack)) 
step (APPLY1:expr) (Cons xs (Cons (Str "zerop") endingStack)) alist = (expr, (Cons (Str "false") endingStack))
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "+") xc))) alist = (y, (Cons (Num(x + s)) xc))
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "*") xc))) alist = (y, (Cons (Num(x * s)) xc))
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "/") xc))) alist = (y, (Cons (Num(x `div` s)) xc))
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "-") xc))) alist = (y, (Cons (Num(x - s)) xc))
step (CONS:y) (Cons x1 (Cons x2 xs)) alist = (y,  (Cons(Cons x2 x1) xs))


printStr "" =  return()
printStr (x:xs) = putChar x >>  printStr xs


--pairlis ["a","b","c"] [1,2,3] -> [("a",1),("b",2),("c",3)]--все списки Cons

-- ((lambda (x y) (+ x y)) 1 2)

-- (null (list)) -> tru
-- EVAL (null x)
-- EVAL null EVAL x APPLY1

-- APPLY1
-- null x

-- ad hoc -- для частного случая

-- TODO применить конечный автомат в польский калькулятор 
-- mental note. Why dont we put in stack


-- (if усл ветка1 ветка2)
-- EVAL усл SWITCH
-- stack: ветка1 ветка2


-- SWITCH
-- stack: true ветка1 ветка2

-- EVAL ветка1
--


-- EVAL (list (+ 1 2) (* 3 4) 3)
--commands: EVAL (+ 1 2) EVAL (* 3 4) EVAL 3 NONE CONS CONS CONS
--COMMANDS: PUSH_NONE EVAL (+ 1 2) CONS EVAL (* 3 4) CONS EVAL 3 CONS
--stack: 3 12 3 None
--stack: 3 12 (3)
--stack: 3 (12 3)
--stack: (3 12 3)  
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


-- EVAL (list (+ 1 2) (* 3 4) 3)
--commands: EVAL (+ 1 2) EVAL (* 3 4) EVAL 3 NONE CONS CONS CONS
--stack: 3 12 3 None
--stack: 3 12 (3)
--stack: 3 (12 3)
--stack: (3 12 3) 