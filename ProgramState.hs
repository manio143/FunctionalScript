{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ProgramState where

import System.IO
import System.Exit
import Debug.Trace

import Control.Monad.Trans.Maybe

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving(Eq, Show)

insertTree :: Ord a => a -> b -> Tree (a,b) -> Tree (a,b)
insertTree key value Leaf = Node Leaf (key, value) Leaf
insertTree key value (Node tl (nk, nv) tr) | key == nk = Node tl (key, value) tr
                                           | key > nk = Node tl (nk, nv) (insertTree key value tr)
                                           | key < nk = Node (insertTree key value tl) (nk, nv) tr

findTree :: Ord a => a -> Tree (a,b) -> Maybe b
findTree key Leaf = Nothing
findTree key (Node tl (nk, nv) tr) | key == nk = Just nv
                                   | key > nk = findTree key tr
                                   | key < nk = findTree key tl

type Ident = String
type Store = Tree (Ident, Either Function Value)
-- Right Function should be used outside of LetExpression with recursive definition

data Number = Int Integer | Float Double
    deriving(Eq, Ord, Show)
type Record = [(Ident, Value)]
type List = [Value]
type Tuple = [Value]
type Union = (Ident, UnionValue)
data UnionValue = UnionEnum | UnionWithValue Value
    deriving(Eq, Ord, Show)
data Function = 
    Func Store Parameter Expression 
    | Constr Ident
    | BuiltIn (Value -> IO Value)

instance Eq Function where
    Func s p e == Func s' p' e' = s == s' && p == p' && e == e'
    _ == _ = False
instance Show Function where
    show (Func s p e) = "(" ++ show p ++") -> " ++ show e
    show (Constr id) = id++"{}"
    show _ = "BuiltIn function"

instance Ord Function where
    f1 <= f2 = False
    f1 >= f2 = False

data Parameter = BoundParameter Ident | WildCard | Unit
    deriving(Eq, Show)

data Value = 
    UnitValue 
    | RecordValue Record 
    | NumberValue Number
    | CharacterValue Char
    | ListValue List 
    | TupleValue Tuple
    | UnionValue Union
    | FunctionValue Function
    deriving(Eq, Ord, Show)

-- data Type = Type [Ident] TypeKind
--     deriving(Eq, Show)

-- data TypeKind =
--     AliasType Ident [Type]
--     | RecordType [(Ident, Type)]
--     | FunctionType Type Type
--     | TupleType [Type]
--     | ListType Type
--     | UnitType
--     | UnionType [(Ident, UnionType)]
--     deriving(Eq, Show)

-- data UnionType = UnionEnumType | UnionWithValueType Type
--     deriving(Eq, Show)

data Expression =
    VariableExpression Ident
    | ApplicationExpression Expression Expression
    | NegativeExpression Expression
    | LetExpression Ident Expression Expression
    | DoExpression Expression Expression
    | ValueExpression Value
    | ListConstruction [Expression]
    | TupleConstruction [Expression]
    | RecordConstruction [(Ident, Expression)]
    | RecordFieldExpression Expression Ident
    | RecordUpdateExpression Ident Expression --constr expr
    | IfExpression Expression Expression Expression
    | IfDoExpression Expression Expression Expression
    | LambdaExpression Parameter Expression
    | MatchExpression Expression [Alternate]
    deriving(Eq, Show)

type Operator = Ident

type Alternate = Value -> Store -> MaybeT IO Expression

instance Eq Alternate where
    _ == _ = False
instance Show Alternate where
    show _ = "alternate"

runProgram :: Store -> [String] -> IO ()
runProgram s args =
    let mmain = getRec "main" s in
    case mmain of
        Nothing -> error "Function `main` not found"
        Just (maine, s') ->
            let argsValue = ListValue $ map stringListToValue args in do
                v <- eval (ApplicationExpression (maine) (VariableExpression "args")) (withVar "args" argsValue $ withRec "main" maine s' s)
                case v of
                    NumberValue i -> 
                        case intOfNumber i of
                            0 -> exitSuccess
                            ii -> exitWith (ExitFailure $ fromIntegral ii)
                    _ -> error "Function `main` returned a non int value"

nativeError :: Value -> IO Value
nativeError v =
    let s = stringListFromValue v in
        error ("ERROR: " ++ s)

stringListFromValue :: Value -> String
stringListFromValue (ListValue l) =
    map charFromValue l
stringListFromValue _ = error "Attempting to convert a non-list value to String"

charFromValue :: Value -> Char
charFromValue (CharacterValue c) = c
charFromValue _ = error "Attempting to convert non character value to Char"

stringListToValue :: String -> Value
stringListToValue = ListValue . map charToValue

charToValue :: Char -> Value
charToValue = CharacterValue

intOfNumber :: Number -> Integer
intOfNumber (Int i) = i
intOfNumber (Float f) = round f


eval :: Expression -> Store ->  IO Value
eval (ValueExpression val) _ = trace ("eval Value of "++show val) return val
eval (NegativeExpression e) s = eval e s >>= neg
    where
        neg (NumberValue (Int i)) = return (NumberValue (Int (-i)))
        neg (NumberValue (Float f)) = return (NumberValue (Float (-f)))
eval (LetExpression id el eo) s = do
    val <- eval el (withRec id el s s)
    eval eo (withVar id val s)
eval (DoExpression ed eo) s = eval ed s >> eval eo s
eval (VariableExpression id) s =
    case getVar id s of
        Just val -> 
            trace ("eval "++id++" - "++ show val) $ return val
        Nothing -> 
            case getRec id s of
                Just (e, s') -> eval e (withRec id e s' s')
                Nothing -> fail ("Variable `"++id++"` is not bound")
eval (ListConstruction eli) s = unpack [] $ map (\e -> eval e s) eli
        where
            unpack acc (h:t) = do
                v <- h
                unpack (v:acc) t
            unpack acc [] = return (ListValue $ reverse acc)
eval (TupleConstruction tup) s = unpack [] $ map (\e -> eval e s) tup
        where
            unpack acc (h:t) = do
                v <- h
                unpack (v:acc) t
            unpack acc [] = return (TupleValue $ reverse acc)
eval (RecordConstruction li) s = unpack [] $ map (\(i,e) -> (i, eval e s)) li
        where
            unpack acc ((i,h):t) = do
                v <- h
                unpack ((i,v):acc) t
            unpack acc [] = return (RecordValue $ reverse acc)
eval (RecordFieldExpression e id) s = do
    val <- eval e s
    case val of
        RecordValue d -> case lookup id d of
                            Nothing -> fail ("Record withour field `"++id++"`")
                            Just v -> return v
eval (RecordUpdateExpression id e) s =
    case getVar id s of
        Nothing -> fail ("Variable `"++id++"` is not bound")
        Just v1 -> case v1 of
                    RecordValue d1 -> do
                        recr <- eval e s
                        case recr of
                            RecordValue d2 -> return $ RecordValue $ merge d1 d2
    where
        merge = foldl (flip insert)
        insert (i, x) ((ii, xx):t) | i == ii = (i,x):t
                                   | otherwise = (ii, xx) : insert (i,x) t
        insert (i, x) [] = [(i,x)]
eval (IfExpression econd etrue efalse) s = do
    cond <- eval econd s
    if cond == true then eval etrue s else eval efalse s
eval (IfDoExpression econd edo econt) s = do
    cond <- eval econd s
    if cond == true then eval edo s >> eval econt s else eval econt s
eval (LambdaExpression p e) s = return $ FunctionValue $ Func s p e
eval (MatchExpression e ali) s = do
    val <- eval e s
    m <- option ali val s
    case m of
        Just ee -> eval ee s
        Nothing -> fail "Unmatched case"
    where
        option :: [Alternate] -> Value -> Store -> IO (Maybe Expression)
        option (f:ft) val s = do
            m <- runMaybeT $ f val s
            case m of
                Nothing -> option ft val s
                Just e -> return $ Just e
eval (ApplicationExpression ef ev) s = do
    fun <- eval ef s
    val <- eval ev s
    case fun of
        FunctionValue (Func s' (BoundParameter x) exp) -> eval exp (withVar x val s')
        FunctionValue (Func s' _ exp) -> eval exp s'
        FunctionValue (BuiltIn f) -> f val
        FunctionValue (Constr id) -> return $ UnionValue (id, UnionWithValue val)

withVar :: Ident -> Value -> Store -> Store
withVar id v = insertTree id (Right v)

getVar :: Ident -> Store -> Maybe Value
getVar id s = case findTree id s of
                Just (Right v) -> Just v
                _ -> Nothing

withRec :: Ident -> Expression -> Store -> Store -> Store
withRec id e env = insertTree id $ Left $ Func env (BoundParameter id) e

getRec :: Ident -> Store -> Maybe (Expression, Store)
getRec id s = case findTree id s of
                Just (Left (Func s p e)) -> Just (e, s)
                _ -> Nothing

builtInOp :: (Value -> Value -> Value) -> Value
builtInOp f = FunctionValue $
                BuiltIn (\a -> return $ FunctionValue $
                    BuiltIn (\b -> return $ f a b))

plus = builtInOp $ numCast (+) (+)
minus = builtInOp $ numCast (-) (-)
times = builtInOp $ numCast (*) (*)
power = builtInOp $ floatCast (**)
divide = builtInOp $ floatCast (/)

numCast :: (Double->Double->Double) -> (Integer->Integer->Integer) -> Value -> Value -> Value
numCast df iff v1 v2 = let v = case (v1, v2) of
                                (NumberValue (Int i), NumberValue (Int j)) -> Int $ iff i j
                                (NumberValue (Int i), NumberValue (Float d)) -> Float $ df (fromInteger i) d
                                (NumberValue (Float d), NumberValue (Int i)) -> Float $ df d (fromInteger i)
                                (NumberValue (Float e), NumberValue (Float d)) -> Float $ df e d
                        in NumberValue v
floatCast :: (Double->Double->Double) -> Value -> Value -> Value
floatCast df v1 v2 = let v = case (v1, v2) of
                                (NumberValue (Int i), NumberValue (Int j)) -> Float $ df (fromInteger i) (fromInteger j)
                                (NumberValue (Int i), NumberValue (Float d)) -> Float $ df (fromInteger i) d
                                (NumberValue (Float d), NumberValue (Int i)) -> Float $ df d (fromInteger i)
                                (NumberValue (Float e), NumberValue (Float d)) -> Float $ df e d
                        in NumberValue v

operatorSet :: [(Ident, Value)]
operatorSet = [
                ("(+)", plus),
                ("(-)", minus),
                ("(*)", times),
                ("(/)", divide),
                ("(**)", power),
                ("(&&)", logicalAnd),
                ("(||)", logicalOr),
                ("(==)", equals),
                ("(<=)", lessEqThan),
                ("(>=)", greaterEqThan),
                ("(++)", concatenate),
                ("id", identityFunction),
                ("ignore", ignoreFunction),
                ("print", printFunction),
                ("True", true),
                ("False", false),
                ("__list_sequence", listSeq),
                ("die", FunctionValue $ BuiltIn nativeError),
                ("head", listHead),
                ("tail", listTail),
                ("(!!)", arrNth)
        ]

equals :: Value
equals = builtInOp $ \a b -> if a == b then true else false

lessEqThan = builtInOp inner
    where
        inner (NumberValue a) (NumberValue b) = if a <= b then true else false
        inner (ListValue l) (ListValue r) = if l <= r then true else false
greaterEqThan = builtInOp inner
    where
        inner (NumberValue a) (NumberValue b) = if a >= b then true else false
        inner (ListValue l) (ListValue r) = if l >= r then true else false

listSeq = builtInOp $ \(NumberValue from) (NumberValue to) -> generate from to
    where
        generate (Int i) (Int j) = mkIntList [i..j]
        generate (Int i) (Float j) = mkFloatList [fromInteger i..j]
        generate (Float i) (Int j) = mkFloatList [i..fromInteger j]
        generate (Float i) (Float j) = mkFloatList [i..j]
        mkIntList = ListValue . map (NumberValue . Int)
        mkFloatList = ListValue . map (NumberValue . Float)

listHead = FunctionValue $ BuiltIn (\(ListValue ls) -> return $ head ls)
listTail = FunctionValue $ BuiltIn (\(ListValue ls) -> return $ ListValue $ tail ls)

arrNth = builtInOp inner
    where 
        inner (TupleValue vs) (NumberValue (Int n)) = vs !! fromInteger n
        inner (ListValue vs) (NumberValue (Int n)) = vs !! fromInteger n
        inner _ _ = error "ERROR: Index out of range"

identityFunction = FunctionValue $ BuiltIn (\a -> return a)
ignoreFunction = FunctionValue $ BuiltIn (\a -> return UnitValue)
printFunction = FunctionValue $ BuiltIn (\a -> print a >> return UnitValue)

concatenate = builtInOp inner
    where
        inner (ListValue l) (ListValue r) = ListValue $ l ++ r

logicalAnd = builtInOp $ \a b -> if a == b && a == true then true else false

logicalOr = builtInOp $ \a b -> if a == true || b == true then true else false

true = UnionValue $ ("True", UnionEnum)
false = UnionValue $ ("False", UnionEnum)

baseLibStore = foldl (\acc (i, f) -> withVar i f acc) Leaf operatorSet

recTest = 
    LetExpression "f" 
        (LambdaExpression (BoundParameter "x") 
            (IfExpression 
                (ApplicationExpression 
                    (ApplicationExpression 
                        (VariableExpression "(==)") 
                        (ValueExpression (NumberValue (Int 0))))
                    (VariableExpression "x"))
                (ValueExpression (NumberValue (Int 0)))
                (ApplicationExpression
                    (ApplicationExpression
                        (VariableExpression "(+)")
                        (ValueExpression (NumberValue (Int 1))))
                    (ApplicationExpression
                        (VariableExpression "f")
                        (ApplicationExpression
                            (ApplicationExpression
                                (VariableExpression "(-)")
                                (VariableExpression "x"))
                            (ValueExpression (NumberValue (Int 1))))))))
        (ApplicationExpression 
            (VariableExpression "f")
            (ValueExpression (NumberValue (Int 5))))