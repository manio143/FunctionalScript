module TypeInferenceSystem where

import AST

import Control.Monad
import Control.Monad.Trans.State
import Data.List ((\\), union, nub, intersect)
import Data.Maybe

import Debug.Trace

import Text.Parsec.Pos (SourcePos, initialPos)

baseLibPos = initialPos "base"

ident :: Ident -> Identifier
ident i = Identifier i baseLibPos

opId :: Op -> SourcePos -> Identifier
opId (Op op) = Identifier ("("++op++")")

-- BUILTIN TYPES
tUnit = TNamed $ ident "()"
tChar = TNamed $ ident "Char"
tNum = TNamed $ ident "Number"
tList = TConstr [ident "a"] (TList baseLibPos (TVar $ ident "a"))
tFun = TConstr [(ident "a"),(ident "b")] (TFunction baseLibPos (TVar $ ident "a") (TVar $ ident "b"))

tString = TAlias (ident "String") $ TApp baseLibPos tList [tChar]

--complex example [a] -> [b]
tComplex = TConstr [(ident "a"),(ident "b")] $ TApp baseLibPos tFun [TApp baseLibPos tList [TVar $ ident "a"], TApp baseLibPos tList [TVar $ ident "b"]]
tInstantiation = TApp baseLibPos tComplex [tChar, tNum]
-- TODO remove the example

type Substitution = [(Ident, Type)]

nullSubst :: Substitution
nullSubst = []

(>->) :: Ident -> Type -> Substitution
u >-> t = [(u, t)]

uids ids = map (\(Identifier i _) -> i) ids

except es ((i,t):ss) | elem i es = except es ss
                     | otherwise = (i,t) : except es ss
except es [] = []

posFail pos msg = fail $ show pos ++ "\n" ++ msg

simplify :: Monad m => Type -> m Type
simplify (TApp pos (TConstr ids tc) ts) | length ids == length ts = do
                                            t <- simplify tc
                                            tss <- mapM simplify ts
                                            return $ zip (uids ids) tss `apply` t
                                        | otherwise = posFail pos "Incorrect number of type parameters"
simplify (TApp pos _ _) = posFail pos "Not a type constructor"
simplify (TConstr ids tc) = return . TConstr ids =<< simplify tc
simplify (TParenthesis t) = simplify t
simplify (TRecord rfts) = return . TRecord =<< mapM (\(RecordFieldType id t) -> return . RecordFieldType id =<< simplify t) rfts
simplify (TFunction pos t1 t2) = do
    ts1 <- simplify t1
    ts2 <- simplify t2
    return $ TFunction pos ts1 ts2
simplify (TTuple ts) = return . TTuple =<< mapM simplify ts
simplify (TList pos t) = return . TList pos =<< simplify t
simplify (TAlias id t) = return . TAlias id =<< simplify t
simplify t = return t

class Types t where
    apply :: Substitution -> t -> t
    typeVariables :: t -> [Ident]

instance Types Type where
    apply s (TVar (Identifier u pos)) = TVar (Identifier u pos) `fromMaybe` lookup u s
    apply s (TFunction pos t1 t2) = TFunction pos (apply s t1) (apply s t2)
    apply s (TTuple ts) = TTuple (map (apply s) ts)
    apply s (TList pos t) = TList pos $ apply s t
    apply s (TParenthesis t) = apply s t
    apply s (TRecord rfts) = TRecord (map (\(RecordFieldType id t) -> RecordFieldType id $ apply s t) rfts)
    apply s (TApp pos t ts) = TApp pos (apply s t) (map (apply s) ts)
    apply s (TConstr ids t) = TConstr ids (apply (except (uids ids) s) t)
    apply s (TAlias id t) = TAlias id (apply s t)
    apply s t = t

    typeVariables (TVar (Identifier u _)) = [u]
    typeVariables (TFunction _ t1 t2) = typeVariables t1 `union` typeVariables t2
    typeVariables (TTuple ts) = foldl union [] $ map typeVariables ts
    typeVariables (TList _ t) = typeVariables t
    typeVariables (TRecord rfts) = foldl union [] $ map (\(RecordFieldType _ t) -> typeVariables t) rfts
    typeVariables (TParenthesis t) = typeVariables t
    typeVariables (TConstr ids t) = typeVariables t \\ uids ids
    typeVariables (TApp _ t ts) = foldl union (typeVariables t) $ map typeVariables ts
    typeVariables (TAlias id t) = typeVariables t
    typeVariables t = []

instance Types a => Types [a] where
    apply s = map (apply s)
    typeVariables = nub . concat . map typeVariables

-- Most general unifier
infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

merge :: Monad m => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
 where agree = all (\v -> apply s1 (TVar $ ident v) == apply s2 (TVar $ ident v))
                   (map fst s1 `intersect` map fst s2)

mergeApply :: Monad m => SourcePos -> Substitution -> [(Type, Type)] -> m Substitution
mergeApply pos l [] = return l
mergeApply pos l ((t, t'):ts) = do
    s <- mgu pos (apply l t) (apply l t')
    let l' = s @@ l
    mergeApply pos l' ts

mgu :: Monad m => SourcePos -> Type -> Type -> m Substitution
mgu pos (TAlias _ t) t' = mgu pos t t'
mgu pos t (TAlias _ t') = mgu pos t t'

mgu pos (TFunction _ t1 t2) (TFunction _ t1' t2') =
    mergeApply pos nullSubst $ zip [t1,t2] [t1', t2']
    -- do
    --     s1 <- mgu t1 t1'
    --     s2 <- mgu (apply s1 t2) (apply s1 t2')
    --     return (s2 @@ s1)
mgu pos (TTuple ts) (TTuple ts') =
    mergeApply pos nullSubst (zip ts ts')

mgu pos (TList _ t) (TList _ t') = mgu pos t t'

mgu pos (TRecord rfts) (TRecord rfts') =
    mergeApply pos nullSubst $ zip (map recFType rfts) (map recFType rfts')
    where
        recFType (RecordFieldType id t) = t

mgu pos (TUnit _) (TUnit _) = return nullSubst
mgu pos (TNamed i) (TNamed i') | i == i' = return nullSubst

mgu pos (TVar (Identifier u _)) t = varBind pos u t
mgu pos t (TVar (Identifier u _)) = varBind pos u t

mgu pos t1 t2 = posFail pos $ expected t2 actual t1 "Types do not unify"

varBind :: Monad m => SourcePos -> Ident -> Type -> m Substitution
varBind pos u t | (case t of
                TVar (Identifier v _) -> v == u
                _ -> False) = return nullSubst
                | u `elem` typeVariables t = posFail pos $ "occurs check fails ("++u++" >-> "++show t ++ " of " ++ show (typeVariables t) ++ ")"
                | otherwise = return (u >-> t)

-- Przypuszczenia
data Assumption = Ident :>: Type deriving (Eq, Show)

instance Types Assumption where
    apply s (i :>: t) = i :>: apply s t
    typeVariables (i :>: t) = typeVariables t

find :: Monad m => Identifier -> [Assumption] -> m Type
find (Identifier id pos) [] = posFail pos ("unbound identifier: " ++ id)
find (Identifier id pos) ((i:>:t):as) | id == i = return t
                                      | otherwise = find (Identifier id pos) as

findMaybe :: Ident -> [Assumption] -> Maybe Type
findMaybe _ [] = Nothing
findMaybe id ((i:>:t):as) | id == i = return t
                          | otherwise = findMaybe id as

-- Inferencja
data InferenceState = InferenceState {subst_ :: Substitution, counter :: Int} deriving (Eq, Show)
initialInferenceState = InferenceState {subst_ = nullSubst, counter = 0}

type TI = StateT InferenceState IO

subst :: TI Substitution
subst = subst_ <$> get

newTVar :: SourcePos -> TI Type
newTVar pos = do
    state <- get
    let n = counter state
    let t = TVar (Identifier ("a"++show n) pos)
    put $ state { counter = n + 1}
    return t

unify :: SourcePos -> Type -> Type -> TI ()
unify pos t1 t2 = do 
    s <- subst
    u <- mgu pos (apply s t1) (apply s t2)
    extSubst u

extSubst s' = do
    state <- get
    s <- subst
    put $ state { subst_ = s' @@ s }

exact :: SourcePos -> Type -> Type -> TI ()
exact pos t1 t2 = do
    st1 <- simplify t1
    st2 <- simplify t2
    if st1 == st2 then return ()
    else posFail pos (expected t2 actual t1 "Types do not match")

expected :: Type -> a -> Type -> String -> String
expected tex _ tact msg = msg ++ "\n\tExpected: "++show tex++"\n\tActual: "++show tact
actual :: ()
actual = ()

tiLit :: Literal -> TI Type
tiLit (Char _) = return tChar
tiLit (String _) = return tString
tiLit (Integer _) = return tNum
tiLit (Float _) = return tNum
tiLit UnitValue = return tUnit

tiExp :: [Assumption] -> Expression -> TI Type
tiExp as (EVariable id) = trace ("lookup "++show id) $ find id as
tiExp as (ELiteral pos lit) = tiLit lit
tiExp as (EParenthesis e) = tiExp as e
tiExp as (ENegative pos e) = do
    t <- tiExp as e
    unify pos t tNum
    return t
tiExp as (EOp pos el op er) = do
    tl <- tiExp as el
    tr <- tiExp as er
    top <- find (opId op pos) as
    simtop <- simplify top
    argsBasedType <- funOf pos [tl,tr] >>= simplify
    unify pos argsBasedType simtop
    return $ funResult simtop 2
tiExp as (ETyped pos e t) = do
    t' <- tiExp as e
    unify pos t' t
    return t'
tiExp as (EApplication pos eapp es) = do
    stap <- tiExp as eapp >>= simplify >>= anonimize pos
    tes <- mapM (tiExp as) es
    argsBasedType <- funOf pos tes >>= simplify >>= anonimize pos
    unify pos (trace (show argsBasedType) argsBasedType) (trace (show stap) stap)
    return $ funResult stap (length es)

anonimize :: SourcePos -> Type -> TI Type
anonimize pos t = do
    let tv = typeVariables t
    s <- mapM (\id -> newTVar pos >>= \n -> return (id, n)) tv
    return $ apply s t

funOf :: SourcePos -> [Type] -> TI Type
funOf pos (t:ts) = TFunction pos t <$> funOf pos ts
funOf pos [] = newTVar pos

funResult :: Type -> Int -> Type
funResult (TFunction _ _ t) n | n == 1 = t
                              | n > 1 = funResult t (n - 1)

tiUnifyExp :: [Assumption] -> (Expression, Type) -> SourcePos -> TI ()
tiUnifyExp as (e, t) pos = do
    t' <- tiExp as e
    unify pos t' t

tiValDecls :: [Assumption] -> [Assumption] -> [ValueDeclaration] -> TI [Assumption]
tiValDecls annotations as ds = do
    ts <- mapM (\(Val (Identifier _ pos) _) -> newTVar pos) ds
    let ids = map (\(Val (Identifier id _) _) -> id) ds
        idPos = map (\(Val (Identifier id pos) _) -> (id, pos)) ds
        as' = zipWith (:>:) ids ts ++as
        exps = map (\(Val _ e) -> e) ds
    zipWithM_ (tiUnifyExp as') (zip exps ts) (map snd idPos)
    s <- subst
    let ts' = apply s ts
        as'' = zipWith (:>:) ids ts'
    mapM_ (\(id :>: t) -> case findMaybe id annotations of
                            Nothing -> return ()
                            Just t' -> do
                                let (Just p) = lookup id idPos
                                unify p t t'
                                exact p t t') as''
    return as''

testValDecl = [
    Val (ident "x") 
        (ENegative baseLibPos $ EVariable (ident "y")),
    Val (ident "y") (ELiteral baseLibPos $ Integer 1),
    Val (ident "z") 
        (EOp baseLibPos (EVariable $ ident "x") (Op "+") (EVariable $ ident "y")),
    Val (ident "h") 
        (EApplication baseLibPos (EVariable $ ident "f") [ELiteral baseLibPos $ Char 'a', ELiteral baseLibPos $ Integer 1, ELiteral baseLibPos $ String "str"]),
    -- Val (ident "mid") 
    --     (EApplication baseLibPos (EVariable (ident "id")) [EVariable (ident "id")]),
    Val (ident "one") 
        (EApplication baseLibPos (EVariable (ident "id")) [ELiteral baseLibPos $ Integer 1])
    ]

testAddOp = "(+)" :>: TFunction baseLibPos tNum (TFunction baseLibPos tNum tNum)
testFunction = "f" :>: TFunction baseLibPos tChar (TFunction baseLibPos tNum (TFunction baseLibPos tString tNum))
testId = "id" :>: TFunction baseLibPos (TVar $ ident "a") (TVar $ ident "a")