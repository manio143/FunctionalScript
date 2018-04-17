{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ProgramState where

import System.IO
import System.Exit

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving(Eq, Show)

type Ident = String
type Store = Tree (Ident, Value)

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
eval (ValueExpression val) _ = return val
eval (NegativeExpression e) s = eval e s >>= neg
    where
        neg (NumberValue (Int i)) = return (NumberValue (Int (-i)))
        neg (NumberValue (Float f)) = return (NumberValue (Float (-f)))
eval (LetExpression id el eo) s = do
    val <- eval el s
    eval eo (withVar id val s)
eval (DoExpression ed eo) s = eval ed s >> eval eo s
eval (VariableExpression id) s =
    case getVar id s of
        Nothing -> error ("Variable `"++id++"` is not bound")
        Just val -> return val
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
    case cond of
        UnionValue ("True", UnionEnum) -> eval etrue s
        UnionValue ("False", UnionEnum) -> eval efalse s
eval (IfDoExpression econd edo econt) s = do
    cond <- eval econd s
    case cond of
        UnionValue ("True", UnionEnum) -> eval edo s >> eval econt s
        UnionValue ("False", UnionEnum) -> eval econt s
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
withVar id v (Leaf) = Node Leaf (id, v) Leaf
withVar id v (Node lt (cid, cv) rt) | id < cid = Node (withVar id v lt) (cid, cv) rt
                                    | id == cid = Node lt (id, v) rt
                                    | id > cid = Node lt (cid, cv) (withVar id v rt)

getVar :: Ident -> Store -> Maybe Value
getVar id Leaf = Nothing
getVar id (Node lt (cid, cv) rt) | id == cid = Just cv
                                 | id < cid = getVar id lt
                                 | id > cid = getVar id rt

arithmetic :: (Double -> Double -> Double) -> Value
arithmetic p = FunctionValue (BuiltIn (\(NumberValue x) -> return $ FunctionValue (BuiltIn (\(NumberValue y) -> return $ NumberValue $ add x y))))
        where
            add (Int i) (Int j) = Int (round $ p (fromInteger i) (fromInteger j))
            add (Int i) (Float j) = Float (p (fromIntegral i) j)
            add (Float i) (Int j) = Float (p i (fromIntegral j))
            add (Float i) (Float j) = Float (p i j)

arithmeticSet :: [(Ident, Value)]
arithmeticSet = [
                ("(+)", arithmetic (+)),
                ("(-)", arithmetic (-)),
                ("(*)", arithmetic (*)),
                ("(/)", arithmetic (/)),
                ("(**)", arithmetic (**))
        ]
