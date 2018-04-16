{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ProgramState where

import System.IO
import System.Exit

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving(Eq, Show)

type Ident = String
type Store = Tree (Ident, Value)

data Number = Int Integer | Float Float
    deriving(Eq, Show)
type Record = [(Ident, Value)]
type List = [Value]
type Tuple = [Value]
type Union = (Ident, UnionValue)
data UnionValue = UnionEnum | UnionWithValue Value
    deriving(Eq, Show)
data Function = Func Store Parameter Expression
    deriving(Eq, Show)

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
    | OpExpression Operator Expression Expression
    | ApplicationExpression Expression Expression
    | NegativeExpression Expression
    | LetExpression Ident Expression Expression
    | DoExpression Expression Expression
    | ValueExpression Value
    | ListConstruction [Expression]
    | TupleConstruction [Expression]
    | RecordConstruction [(Ident, Expression)]
    | RecordFieldExpression Expression Ident
    | RecordUpdateExpression Ident Value
    | IfExpression Expression Expression Expression
    | IfDoExpression Expression Expression Expression
    | LambdaExpression Parameter Expression
    | MatchExpression Expression [Alternate]
    -- | SideEffectsExpression (IO Value)
    deriving(Eq, Show)

type Operator = Ident

type Alternate = Expression -> Maybe Expression

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
eval _ _ = return UnitValue --TODO

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
