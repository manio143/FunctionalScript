{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ProgramState where

import System.IO
import System.Exit
import Debug.Trace
import Text.Read

import Data.List
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
    deriving(Eq, Ord)

instance Show Number where
    show (Int i) = show i
    show (Float f) = show f

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
    | Special Ident

instance Eq Function where
    Func s p e == Func s' p' e' = s == s' && p == p' && e == e'
    _ == _ = False
instance Show Function where
    show (Func s p e) = show p ++" -> " ++ show e
    show (Constr id) = id++"{}"
    show _ = "BuiltIn function"

instance Ord Function where
    f1 <= f2 = False
    f1 >= f2 = False

data Parameter = BoundParameter Ident | WildCard | Unit
    deriving(Eq)

instance Show Parameter where
    show (BoundParameter id) = id
    show WildCard = "_"
    show Unit = "()"

data Value = 
    UnitValue 
    | RecordValue Record 
    | NumberValue Number
    | CharacterValue Char
    | ListValue List 
    | TupleValue Tuple
    | UnionValue Union
    | FunctionValue Function
    deriving(Eq, Ord)

instance Show Value where
    show UnitValue = "()"
    show (RecordValue rc) = "{ "++intercalate "; " (map (\(id,val) -> id++" = "++show val) rc)++" }"
    show (NumberValue n) = show n
    show (CharacterValue c) = "'"++[c]++"'"
    show (ListValue []) = "[]"
    show (ListValue l) = case head l of
                            CharacterValue{} -> "\""++map (\(CharacterValue c)->c) l++"\""
                            _ -> "["++intercalate ", " (map show l)++"]"
    show (TupleValue l) = "("++intercalate ", " (map show l)++")"
    show (UnionValue (id, UnionEnum)) = id
    show (UnionValue (id, UnionWithValue v)) = id++"("++show v++")"
    show (FunctionValue f) = show f

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
    deriving(Eq)

instance Show Expression where
    show (VariableExpression id) = id
    show (ApplicationExpression e1 e2) = show e1 ++ "("++show e2++")"
    show (NegativeExpression e) = "- "++show e
    show (LetExpression id el ec) = "let "++id++" = "++show el++" in "++show ec
    show (DoExpression el ec) = "do "++show el++" then "++show ec
    show (ValueExpression val) = show val
    show (ListConstruction es) = "["++intercalate ", " (map show es)++"]"
    show (TupleConstruction es) = "("++intercalate ", " (map show es)++")"
    show (RecordConstruction es) = "{"++intercalate "; " (map (\(id,e) ->id++" = "++show e) es)++"}"
    show (RecordUpdateExpression id (RecordConstruction es)) = "{ "++id++" with "++ intercalate "; " (map (\(id,e) ->id++" = "++show e) es)++"}"
    show (RecordFieldExpression e id) = show e ++ "." ++ id
    show (IfExpression ec et ef) = "if "++show ec++" then "++show et++" else "++show ef
    show (IfDoExpression ec et ef) = "if "++show ec++" do "++show et++" then "++show ef
    show (LambdaExpression p e) = show p++" -> "++show e
    show (MatchExpression e _) = "match "++show e++" with (...)"
    

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
        Nothing -> errorT "Function `main` not found"
        Just (maine, s') ->
            let argsValue = ListValue $ map stringListToValue args in do
                v <- eval (ApplicationExpression (maine) (VariableExpression "args")) (withVar "args" argsValue $ withRec "main" maine s' s)
                case v of
                    NumberValue i -> 
                        case intOfNumber i of
                            0 -> exitSuccess
                            ii -> exitWith (ExitFailure $ fromIntegral ii)
                    _ -> errorT "Function `main` returned a non int value"

nativeError :: Value -> IO Value
nativeError v =
    let s = stringListFromValue v in
        errorT s

errorT :: String -> IO a
errorT msg = do
    hPutStrLn stderr $ "ERROR: "++msg
    error "Terminated."

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
eval (ValueExpression val) _ = return val
eval (NegativeExpression e) s = eval e s >>= neg
    where
        neg (NumberValue (Int i)) = return (NumberValue (Int (-i)))
        neg (NumberValue (Float f)) = return (NumberValue (Float (-f)))
eval (LetExpression id el eo) s = do
    val <- eval el (withRec id el s s)
    eval eo (withVar id val s)
eval (DoExpression ed eo) s = eval ed s >> eval eo s
eval (VariableExpression id) s =
    case  getVar id s of
        Just val -> 
            return val
        Nothing -> 
            case getRec id s of
                Just (e, s') -> eval e (withRec id e s' s')
                Nothing -> errorT ("Variable `"++id++"` is not bound")
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
                            Nothing -> errorT ("Record without field `"++id++"`")
                            Just v -> return v
eval (RecordUpdateExpression id e) s =
    case getVar id s of
        Nothing -> errorT ("Variable `"++id++"` is not bound")
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
        Nothing -> errorT "Unmatched case"
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
        FunctionValue (Special id) -> case id of
                                        "__dump_store" -> dumpStore s

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

updateRec :: [Ident] -> Store -> Store -> Store
updateRec ids env s = 
    foldl (\s' id -> let Just (e,_) = getRec id s' in withRec id e env s') s ids

builtInOp :: (Value -> Value -> Value) -> Value
builtInOp f = FunctionValue $
                BuiltIn (\a -> return $ FunctionValue $
                    BuiltIn (\b -> return $ f a b))

plus = builtInOp $ numCast (+) (+)
minus = builtInOp $ numCast (-) (-)
times = builtInOp $ numCast (*) (*)
power = builtInOp $ floatCast (**)
divide = builtInOp $ floatCast (/)
modulus = builtInOp $ intCast mod

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
intCast :: (Integer->Integer->Integer) -> Value -> Value -> Value
intCast df v1 v2 = let v = case (v1, v2) of
                                (NumberValue (Int i), NumberValue (Int j)) -> Int $ df i j
                                (NumberValue (Int i), NumberValue (Float d)) -> Int $ df i (round d)
                                (NumberValue (Float d), NumberValue (Int i)) -> Int $ df (round d) i
                                (NumberValue (Float e), NumberValue (Float d)) -> Int $ df (round e) (round d)
                        in NumberValue v
operatorSet :: [(Ident, Value)]
operatorSet = [
                ("(+)", plus),
                ("(-)", minus),
                ("(*)", times),
                ("(/)", divide),
                ("(**)", power),
                ("(%)", modulus),
                ("(&&)", logicalAnd),
                ("(||)", logicalOr),
                ("(==)", equals),
                ("(<=)", lessEqThan),
                ("(>=)", greaterEqThan),
                ("(++)", concatenate),
                ("(:)", append),
                ("id", identityFunction),
                ("ignore", ignoreFunction),
                ("print", printFunction),
                ("printStr", printStrFunction),
                ("True", true),
                ("False", false),
                ("__list_sequence", listSeq),
                ("die", FunctionValue $ BuiltIn nativeError),
                ("head", listHead),
                ("tail", listTail),
                ("(!!)", arrNth),
                ("toString", toStr),
                ("toNum", toNum),
                ("readln", readLine),
                ("map", mapFunction),
                ("__map", mapFunction),
                ("flatten", flatFunction),
                ("__dump_store", FunctionValue $ Special "__dump_store")
        ]

equals :: Value
equals = builtInOp $ \a b -> if a == b then true else false

append = builtInOp $ \a (ListValue l) -> ListValue (a:l)

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

listHead = FunctionValue $ BuiltIn inner
    where
        inner (ListValue (a:as)) = return a
        inner (ListValue []) = errorT "Head of empty list"
listTail = FunctionValue $ BuiltIn inner
    where
        inner (ListValue (a:as)) = return $ ListValue as
        inner (ListValue []) = errorT "Tail of empty list"                  

arrNth = FunctionValue $
            BuiltIn (\a -> return $ FunctionValue $
                BuiltIn (\b -> inner a b))
    where 
        inner (TupleValue vs) (NumberValue (Int n)) | fromInteger n < length vs = return $ vs !! fromInteger n
        inner (ListValue vs) (NumberValue (Int n)) | fromInteger n < length vs = return $ vs !! fromInteger n
        inner _ _ = errorT "Index out of range"

toStr = FunctionValue $ BuiltIn $ \a -> return $ stringListToValue $ show a

identityFunction = FunctionValue $ BuiltIn (\a -> return a)

ignoreFunction = FunctionValue $ BuiltIn (\a -> return UnitValue)

printFunction = FunctionValue $ BuiltIn (\a -> print a >> return UnitValue)
printStrFunction = FunctionValue $ BuiltIn (\a -> putStrLn (stringListFromValue a) >> return UnitValue)

toNum = FunctionValue $ BuiltIn 
    $ \s -> let str = stringListFromValue s in
                case readMaybe str :: Maybe Integer of
                    Just ii -> return $ NumberValue $ Int ii
                    Nothing -> 
                        case readMaybe str :: Maybe Double of
                            Just ff -> return $ NumberValue $ Float ff
                            Nothing -> errorT "String doesn't represent a number"

readLine = FunctionValue $ BuiltIn
    $ \_ -> getLine >>= return . stringListToValue

mapFunction =  FunctionValue $
                    BuiltIn (\a -> return $ FunctionValue $
                        BuiltIn (\b -> inner a b))
        where
            inner f (ListValue l) = do
                l' <- mapM (\v -> eval (ApplicationExpression
                                        (ValueExpression f)
                                        (ValueExpression v)) Leaf) l
                return $ ListValue l'

flatFunction = FunctionValue $ BuiltIn
    $ \(ListValue l) -> 
        case l of
            [] -> return $ ListValue []
            (ListValue{}:ls) -> return $ ListValue $ concat $ map (\(ListValue lv) -> lv) l
            _ -> return $ ListValue l
        

concatenate = builtInOp inner
    where
        inner (ListValue l) (ListValue r) = ListValue $ l ++ r

logicalAnd = builtInOp $ \a b -> if a == b && a == true then true else false

logicalOr = builtInOp $ \a b -> if a == true || b == true then true else false

true = UnionValue $ ("True", UnionEnum)
false = UnionValue $ ("False", UnionEnum)

baseLibStore = foldl (\acc (i, f) -> withVar i f acc) Leaf operatorSet

dumpStore :: Store -> IO Value
dumpStore s = do
    mapM_ (\v -> print v) $ inf s
    return UnitValue
    where
        inf Leaf = []
        inf (Node tl v tr) = inf tl ++ [v] ++ inf tr
