import Data.Char
import Data.Int
import Debug.Trace
import Data.Time.Format.ISO8601 (yearFormat)

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
data Command = EVAL List|DROP|RET Int|APPLY2|APPLY1|CONS|PUSH_NONE|SWITCH|APPLY_LAMBDA deriving Show




--392 = 3*10^2+9*10^1+2*10^0
--357%10= 7
--357/10=35
--35%10=5
--35/10=3
--3%10 = 3
--3/10 = 0

zeroToStr 0 [] = ['0']
zeroToStr x temp = intToStr x temp


intToStr :: Int -> [Char] -> [Char]
intToStr 0 x = x
intToStr n temp = intToStr (n`div`10) ((chr (n`mod`10+48)):temp)

consToStr None = ""
consToStr (Cons x None) = (lstToStr x)
consToStr (Cons x xs) = (lstToStr x)++ " " ++ (consToStr xs)

lstToStr None = ""
lstToStr (Cons xs xy) = "("++consToStr(Cons xs xy)++")"
lstToStr (Num n) = (zeroToStr n "")
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

callEval [] stack alist globalVar = return()
callEval expr stack alist globalVar = (printStr ("expr: "++show (printExpr expr')++"\n"))>>
  (printStr ("Stack: "++lstToStr stack'++"\n"))>>  
  (printStr("alist: "++(printAlist alist')++"\n"))>>  
  (printStr("globalVar: "++(printAlist globalVar')++"\n"))>>
  callEval expr' stack' alist' globalVar'
  where (expr', stack', alist', globalVar') = step expr stack alist globalVar
    

doubleElinFunc x 0 = []
doubleElinFunc x n = x:doubleElinFunc x (n-1)
 
assoc :: [Char]->[([Char], List)]->List
assoc keyFind ((key,value):xs) = if keyFind == key then value else assoc keyFind xs
 
assocBool :: [Char]->[([Char], List)]->Bool
assocBool keyFind ((key,value):xs) = if keyFind == key then True else assocBool keyFind xs
assocBool keyFind [] = False

--pairlis ["a","b","c"] [1,2,3] -> [("a",1),("b",2),("c",3)]--все списки Cons
--Cons(Cons "a")

printAlist [] = ""
printAlist ((key,value):xs) = (key++" "++(lstToStr value)++", ")++printAlist xs

drop' 0 xs = xs
drop' num (x:xs) = drop' (num - 1) xs


pairList :: List->List->[([Char],List)]->[([Char],List)]
pairList None None ascLst = ascLst
pairList (Cons (Str x) xs) (Cons  y ys) ascLst  =  (x,y):(pairList xs ys ascLst)

appendToExprWithEvalDrop :: List->[Command]
appendToExprWithEvalDrop None = []
appendToExprWithEvalDrop (Cons x None)  = [EVAL x]
appendToExprWithEvalDrop (Cons x xs) = (EVAL x):DROP: appendToExprWithEvalDrop xs

step :: [Command]->List->[([Char],List)]->[([Char], List)]->([Command], List,[([Char],List)], [([Char], List)])
step [] stack alist globalVar = ([], stack, alist, globalVar)
step (PUSH_NONE:y) stack alist globalVar =  (y, (Cons None stack), alist, globalVar)         --actualArg - it (2+3); bodyFunc - it (* x y)
step (EVAL (Cons (Cons (Str "lambda")(Cons formalArg (Cons bodyFunc None))) actualArg):endingExpr) stack alist globalVar = (((appendToExprWithEval actualArg)++[PUSH_NONE]++(doubleElinFunc CONS (lenghtList actualArg))++[APPLY_LAMBDA]++endingExpr),(Cons(Cons (Str "lambda")(Cons formalArg (Cons bodyFunc None))) stack), alist, globalVar)
step (EVAL (Str s):xs) stack alist globalVar|assocBool s globalVar = trace("GLOBAL VAR = " ++ show globalVar)(xs, (Cons (assoc s globalVar) stack), alist, globalVar)
step (EVAL (Str s):xs) stack alist globalVar = (xs, (if assocBool s alist then  (Cons (assoc s alist) stack) else Cons (Str s) stack), alist, globalVar)
step (EVAL (Num x):xs) stack  alist globalVar =  ( xs, (Cons (Num x) stack), alist, globalVar)
step (EVAL (Cons (Str "list") xs):y) stack alist globalVar = (((appendToExprWithEval xs)++[PUSH_NONE]++(doubleElinFunc CONS (lenghtList xs))++y),stack, alist, globalVar)
step (EVAL (Cons (Str "progn") xs):y) stack alist globalVar = ((appendToExprWithEvalDrop xs)++y, stack, alist, globalVar)
step (EVAL (Cons (Str "define")(Cons  (Str var) (Cons value None))):y) stack alist globalVar = (y, (Cons (Str var) stack), alist, (var, value):globalVar)
step (EVAL (Cons (Str var) (Cons value None)):y) stack alist globalVar|assocBool var globalVar = ((EVAL(Cons (assoc var globalVar) (Cons value None)):y), stack, alist, globalVar)
step (EVAL (Cons (Str "if")( Cons cond (Cons branch1 (Cons branch2 None)))):expr) stack alist globalVar= ((EVAL cond:SWITCH:expr) , ((Cons branch1(Cons branch2 stack))), alist, globalVar)
step (SWITCH:expr)  (Cons (Str "true")(Cons branch1(Cons branch2 endingStack))) alist globalVar= ((EVAL branch1):expr, endingStack, alist, globalVar)
step (SWITCH:expr)  (Cons (Str "false")(Cons branch1(Cons branch2 endingStack))) alist globalVar = ((EVAL branch2):expr, endingStack, alist, globalVar)
step (EVAL (Cons(Str string)(Cons xs None)):expr) stack alist globalVar = (((EVAL (Str string)):(EVAL xs):APPLY1:expr), stack, alist, globalVar)--описаны сразу 2 шаблона для null and zerop
step (EVAL (Cons x xy):xs ) stack alist globalVar = (((appendToExprWithEval (Cons x xy))++[APPLY2]++xs), stack, alist, globalVar)
step (DROP:expr) (Cons topStack remainsStack) alist globalVar = (expr, remainsStack, alist, globalVar)
step (APPLY_LAMBDA:expr)(Cons actualArg (Cons(Cons (Str "lambda")(Cons formalArg (Cons bodyFunc None))) endingStack)) alist globalVar = ((EVAL bodyFunc):(RET(lenghtList actualArg)):expr, endingStack ,(pairList formalArg actualArg alist), globalVar)
step (APPLY1:expr) (Cons None (Cons (Str "null") endingStack)) alist globalVar = (expr, (Cons (Str "true") endingStack), alist, globalVar)
step (RET num:expr)  stack alist globalVar = (expr, stack, (drop' num alist), globalVar)
step (APPLY1:expr) (Cons xs (Cons (Str "null") endingStack)) alist globalVar = (expr, (Cons (Str "false") endingStack), alist, globalVar)
step (APPLY1:expr) (Cons (Num 0) (Cons (Str "zerop") endingStack)) alist globalVar = (expr, (Cons (Str "true") endingStack), alist, globalVar) 
step (APPLY1:expr) (Cons xs (Cons (Str "zerop") endingStack)) alist globalVar = (expr, (Cons (Str "false") endingStack), alist, globalVar)
step (APPLY1:expr) (Cons (Cons x xs)(Cons (Str "car") endingStack)) alist globalVar = (expr, (Cons x endingStack), alist, globalVar)
step (APPLY1:expr) (Cons (Cons x xs)(Cons (Str "cdr") endingStack)) alist globalVar = (expr, (Cons xs endingStack), alist, globalVar)
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "+") xc))) alist globalVar = (y, (Cons (Num(x + s)) xc), alist, globalVar)
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "*") xc))) alist globalVar = (y, (Cons (Num(x * s)) xc), alist, globalVar)
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "/") xc))) alist globalVar = (y, (Cons (Num(x `div` s)) xc), alist, globalVar)
step (APPLY2:y) (Cons (Num s) (Cons (Num x) (Cons (Str "-") xc))) alist globalVar = (y, (Cons (Num(x - s)) xc), alist, globalVar)
step (CONS:y) (Cons x1 (Cons x2 xs)) alist globalVar = (y,  (Cons(Cons x2 x1) xs), alist, globalVar)


check x y| x==True  =trace("CHECK = " ++ show x) x

--(+ ((lambda (x) x) 3) x)

--берем переменную и значение и их клаlем в globalVar

--TODO написать car и  cudr  и подумать какие команды понадобятся для выполнения  cond - APPLY2 and EVAL
--(cond ((a b) (c d) (e f)) = (if a b (if c d (if e f))

--EVAL CONS)

--(1 2 3)
--(cons 1 (cons 2 (cons 3)))
--(cons 1 (cons (cons 2 (cons 3 npne))))

--step (EVAL (Cons (Cons (Str "lambda")(Cons formalArg bodyFunc)) actualArg):endingExpr) stack alist =(((appendToExprWithEval bodyFunc)++[PUSH_NONE]++(doubleElinFunc CONS (lenghtList bodyFunc))++[APPLY_LAMBDA]++endingExpr), (Cons actualArg(Cons bodyFunc(Cons actualArg stack))), alist)

--([EVAL (Cons (Cons (Str "*") (Cons (Str "x") (Cons (Str "y") None))) None)],Cons (Cons (Str "x") (Cons (Str "y") None)) (Cons (Cons (Num 2) (Cons (Num 3) None)) None),*** Exception: lisp.hs:(137,1)-(138,65): Non-exhaustive patterns in function pairList
-- (progn (+ 5 1) (print 2) (print 3))
-- (progn (define f (lambda (x) (+ 1 x))) (f 3)) -> 4

printExpr ::[Command]->[Char]
printExpr [] = ""
printExpr (PUSH_NONE:remains) = " PUSH_NONE "++(printExpr remains)
printExpr ((EVAL arg):remains) = (" EVAL "++lstToStr arg)++(printExpr remains)
printExpr (SWITCH:remains) = " SWITCH "++(printExpr remains)
printExpr (APPLY1:remains) = " APPLY1 "++(printExpr remains)
printExpr (APPLY2:remains) = " APPLY2 "++(printExpr remains)
printExpr (APPLY_LAMBDA:remains) = " APPLY_LAMBDA "++(printExpr remains)
printExpr (CONS:remains) = " CONS "++(printExpr remains)
printExpr ((RET arg):remains) = " RET"++(printExpr remains)
printExpr (DROP:remains) = " DROP"++(printExpr remains)




printStr "" =  return()
printStr (x:xs) = putChar x >>  printStr xs


var = callEval [EVAL (parse (tokenize "(progn (define lenghtList (lambda (lst) (if (null lst) 0 (+(lenghtList (cdr lst))1)))) (lenghtList (list 3 3)))" ) None)] None [("a", (Cons (Num 42) None))][("x",(Num 3)), ("d", (Num 4))]  


--TODO читать портянку, пока не дойдет как работает стековая машина
--1. пропадает * x y его нужно положить на стек, что бы потом APPLY_LAMBDA сняло его со стека
--2.   не хватает каманды APPLY_LAMBDA
--3. порядок значений после вычисления 143 я line и пропистать его в APPLY_LAMBDA
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