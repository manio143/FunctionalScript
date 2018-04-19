{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ProgramState where

import System.IO
import System.Exit
import Debug.Trace

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving(Eq, Show)

type Ident = String
type Store = Tree (Ident, Either Function Value)

data Number = Int Integer | Float Double
    deriving(Eq, Show)
type Record = [(Ident, Value)]
type List = [Value]
type Tuple = [Value]
type Union = (Ident, UnionValue)
data UnionValue = UnionEnum | UnionWithValue Value
    deriving(Eq, Show)
data Function = Func Store Parameter Expression | BuiltIn (Value -> IO Value)

instance Eq Function where
    Func s p e == Func s' p' e' = s == s' && p == p' && e == e'
    _ == _ = False
instance Show Function where
    show (Func s p e) = show s ++ " (" ++ show p ++") -> " ++ show e
    show _ = "BuiltIn function"

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
    deriving(Eq, Show)

data Type = Type [Ident] TypeKind
    deriving(Eq, Show)

data TypeKind =
    AliasType Ident --params
    | RecordType [(Ident, Type)]
    | FunctionType Type Type
    | TupleType [Type]
    | ListType Type
    | TUnit
    | UnionType [(Ident, UnionType)]
    deriving(Eq, Show)

data UnionType = UnionEnumType | UnionWithValueType Type
    deriving(Eq, Show)

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
    -- | SideEffectsExpression (IO Value)
    deriving(Eq, Show)

type Operator = Ident

type Alternate = Value -> Maybe Expression

instance Eq Alternate where
    _ == _ = False
instance Show Alternate where
    show _ = "alternate"

runProgram :: Store -> [String] -> IO ()
runProgram s args =
    let mmain = getVar "main" s in
    case mmain of
        Nothing -> error "Function `main` not found"
        Just main ->
            let argsValue = ListValue $ map stringListToValue args in do
                v <- eval (ApplicationExpression (ValueExpression main) (VariableExpression "args")) (withVar "args" argsValue s)
                case v of
                    NumberValue i -> 
                        case intOfNumber i of
                            0 -> exitSuccess
                            ii -> exitWith (ExitFailure $ fromIntegral ii)
                    _ -> error "Function `main` returned a non int value"

nativeError :: Store -> Value -> IO (Value, Store)
nativeError _ v =
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


eval :: Expression -> Store -> IO Value
eval (ValueExpression val) _ = trace ("eval val "++show val) return val
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
        Just val -> trace ("eval "++id++" - "++ show val) $ return val
        Nothing -> 
            case getRec id s of
                Just (e, s') -> eval e (withRec id e s' s')
                Nothing -> error ("Variable `"++id++"` is not bound")
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
                            Nothing -> error ("Record withour field `"++id++"`")
                            Just v -> return v
eval (RecordUpdateExpression id e) s =
    case getVar id s of
        Nothing -> error ("Variable `"++id++"` is not bound")
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
    case option ali val of
        Just ee -> eval ee s
        Nothing -> error "Unmatched case"
    where
        option (f:ft) val =
            case f val of
                Nothing -> option ft val
                Just e -> Just e
eval (ApplicationExpression ef ev) s = do
    fun <- eval ef s
    val <- eval ev s
    case fun of
        FunctionValue (Func s' (BoundParameter x) exp) -> eval exp (withVar x val s')
        FunctionValue (Func s' _ exp) -> eval exp s'
        FunctionValue (BuiltIn f) -> f val

withVar :: Ident -> Value -> Store -> Store
withVar id v Leaf = Node Leaf (id, Right v) Leaf
withVar id v (Node lt (cid, cv) rt) | id < cid = Node (withVar id v lt) (cid, cv) rt
                                    | id == cid = Node lt (id, Right v) rt
                                    | id > cid = Node lt (cid, cv) (withVar id v rt)

getVar :: Ident -> Store -> Maybe Value
getVar id Leaf = Nothing
getVar id (Node lt (cid, gcv) rt) | id == cid = case gcv of
                                                    Right v -> Just v
                                                    Left{} -> Nothing
                                  | id < cid = getVar id lt
                                  | id > cid = getVar id rt

withRec :: Ident -> Expression -> Store -> Store -> Store
withRec id e env Leaf = Node Leaf (id, Left $ Func env (BoundParameter id) e) Leaf
withRec id e env (Node lt (cid, cv) rt) | id < cid = Node (withRec id e env lt) (cid, cv) rt
                                    | id == cid = Node lt (id, Left $ Func env (BoundParameter id) e) rt
                                    | id > cid = Node lt (cid, cv) (withRec id e env rt)

getRec :: Ident -> Store -> Maybe (Expression, Store)
getRec id Leaf = Nothing
getRec id (Node lt (cid, Left (Func s p e)) rt) | id == cid = Just (e, s)
getRec id (Node lt (cid, _) rt) | id < cid = getRec id lt
                                | id > cid = getRec id rt

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
                ("(==)", equals)
        ]

equals :: Value
equals = builtInOp $ \a b -> if a == b then true else false


true = UnionValue $ ("True", UnionEnum)
false = UnionValue $ ("False", UnionEnum)

basicStore = foldl (\acc (i, f) -> withVar i f acc) Leaf operatorSet

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