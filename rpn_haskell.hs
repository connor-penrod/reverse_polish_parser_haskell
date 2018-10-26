module HW4 where
import Prelude hiding (Either, Left, Right)

data Op = Val Int | Plus | Minus | Mul | IntDiv deriving ( Show , Eq )
type PExp = [Op]

-- PROBLEM 1

rpnParse :: String -> PExp
rpnParse str = lParse (words str)
    where lParse [] = []
          lParse (value:values) = (cParse value) : lParse values
            where cParse quantity
                    | quantity == "+" = Plus
                    | quantity == "-" = Minus
                    | quantity == "*" = Mul
                    | quantity == "/" = IntDiv
                    | otherwise = Val (read(quantity) :: Int)
                    
-- PROBLEM 2

consumeVal (Val x) = x

helpEval :: PExp -> [Op]
helpEval xs = (foldl opEval [] xs)
    where opEval (x:y:ys) Plus       = (Val(consumeVal(x) + consumeVal(y))):ys
          opEval (x:y:ys) Minus      = (Val(consumeVal(y) - consumeVal(x))):ys
          opEval (x:y:ys) Mul        = (Val(consumeVal(x) * consumeVal(y))):ys
          opEval (Val 0:y:ys) IntDiv = error "DivByZero"
          opEval (x:y:ys) IntDiv     = (Val(consumeVal(y) `quot` consumeVal(x))):ys
          opEval xs (Val value)            = (Val value):xs
          opEval _ _ = error "Invalid PExp"
  
eval :: PExp -> Int
eval []  = error "Bad input"
eval xs
    | (length (helpEval xs)) /= 1 = error "Invalid PExp"
    | otherwise = consumeVal(head $ helpEval xs)
    
-- PROBLEM 3 
 
data RPNError = DivByZero | InvalidInput deriving ( Show , Eq )
data Either a b = Left a | Right b deriving ( Show , Eq )
type RPNResult = Either RPNError Int
type RPNResultOp = Either RPNError Op

consumeValSafe (Right(Val x)) = Right x

help2Eval :: PExp -> [RPNResultOp]
help2Eval xs = (foldl opEval [] xs)
    where opEval (Right x:Right y:ys) Plus       = (Right (Val(consumeVal(x) + consumeVal(y)))):ys
          opEval (Right x:Right y:ys) Minus      = (Right (Val(consumeVal(y) - consumeVal(x)))):ys
          opEval (Right x:Right y:ys) Mul        = (Right (Val(consumeVal(x) * consumeVal(y)))):ys
          opEval (Right (Val 0):y:ys) IntDiv     = (Left DivByZero):ys
          opEval (Right x:Right y:ys) IntDiv     = (Right (Val(consumeVal(y) `quot` consumeVal(x)))):ys
          opEval xs (Val value)                  = (Right (Val value)):xs
          opEval xs value                        = (Left InvalidInput):xs

checkForDivZero [] = False
checkForDivZero (x:[])
    | x == Left DivByZero = True
    | otherwise = False
checkForDivZero (x:xs)
    | x == Left DivByZero = True
    | otherwise = checkForDivZero xs
         
evalSafe :: PExp -> RPNResult
evalSafe []  = Left InvalidInput
evalSafe xs
    | checkForDivZero (help2Eval xs) = Left DivByZero
    | (length (help2Eval xs)) /= 1 = Left InvalidInput
    | consumeValSafe(head $ help2Eval xs) == Left InvalidInput = Left InvalidInput
    | otherwise = consumeValSafe(head $ help2Eval xs)
    
-- PROBLEM 4
 
type RPNResultStr = Either String String

help3Eval :: PExp -> [RPNResultStr]
help3Eval xs = (foldl opEval [] xs)
    where opEval (Right x:Right y:ys) Plus = (Right ("(" ++ y ++ " + " ++ x ++ ")")):ys
          opEval (Right x:Right y:ys) Minus      = (Right ("(" ++ y ++ " - " ++ x ++ ")")):ys
          opEval (Right x:Right y:ys) Mul        = (Right ("(" ++ y ++ " * " ++ x ++ ")")):ys
          opEval (Right "0":y:ys) IntDiv = (Left "Can't divide by zero"):ys
          opEval (Right x:Right y:ys) IntDiv     = (Right ("(" ++ y ++ " / " ++ x ++ ")")):ys
          opEval xs (Val value)            = (Right (show value)):xs
          opEval xs op            = (Left "Invalid input"):xs

checkForDivZero2 [] = False
checkForDivZero2 (x:[])
    | x == Left "Can't divide by zero" = True
    | otherwise = False
checkForDivZero2 (x:xs)
    | x == Left "Can't divide by zero" = True
    | otherwise = checkForDivZero2 xs
         
rpnTrans :: PExp -> RPNResultStr
rpnTrans []  = Left "Invalid input"
rpnTrans xs
    | checkForDivZero2 (help3Eval xs) = Left "Can't divide by zero"
    | (length (help3Eval xs)) /= 1 = Left "Invalid input"
    | otherwise = (head $ help3Eval xs)
    
