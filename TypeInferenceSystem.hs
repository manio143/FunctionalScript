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
tBool = TNamed $ ident "Bool"
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

mgu pos (TRecord rfts) (TRecord rftsExpected) = do
    let pairsM = map (\(id, t) -> (id, t, lookup id $ map recFType rfts)) $ map recFType rftsExpected
    trace (show pairsM ++ " of "++show rftsExpected ++ " -> "++show rfts) mapM_ (\(id, t, mb) -> case mb of
                            Nothing -> posFail pos $ "Record doesn't have field `"++id++"`"
                            _ -> return () ) pairsM
    let pairs = map (\(id, t, Just t') -> (t,t')) pairsM
    mergeApply pos nullSubst pairs
    where
        recFType (RecordFieldType (Identifier id _) t) = (id, t)

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
data InferenceState = InferenceState {
        subst_ :: Substitution,
        counter :: Int,
        recordStack :: [Assumption]
    } deriving (Eq, Show)
initialInferenceState = InferenceState {subst_ = nullSubst, counter = 0, recordStack = []}

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
    trace ("Unify actual "++show (apply s t1)++" with expected "++show (apply s t2)) return ()
    u <- mgu pos (apply s t1) (apply s t2)
    extSubst u

extSubst s' = do
    state <- get
    s <- subst
    put $ state { subst_ = s' @@ s }

clearSubst action = do
    s <- subst
    a <- action
    state <- get
    put $ state { subst_ = s}
    return a

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

applySubst :: Types t => t -> TI t
applySubst t = do
    s <- subst
    return $ apply s t {-trace ("APPLY "++show s)-} 

pushRecordField :: Type -> Type -> TI ()
pushRecordField (TVar (Identifier id _)) fieldType = do
    state <- get
    put $ state{recordStack = (id :>: fieldType) : recordStack state}

popRecordField :: Type -> TI Type
popRecordField (TVar (Identifier id pos)) = do
    state <- get
    let fieldTypes = filter (\(i:>:_) -> i == id) $ recordStack state
    trace ("Pop record of "++show fieldTypes) return ()
    put $ state { recordStack = recordStack state \\ fieldTypes}
    if null fieldTypes then posFail pos "Cannot create a record from empty list"
    else makeRfts pos fieldTypes [] >>= return . TRecord
    where
        makeRfts :: SourcePos -> [Assumption] -> [(String, Type)] -> TI [RecordFieldType]
        makeRfts pos ((id :>: TRecord [RecordFieldType (Identifier fid pos') t]):ts) acc = 
            case lookup fid acc of
                    Nothing -> makeRfts pos ts ((fid,t):acc)
                    Just t' -> do
                        unify pos' t t'
                        makeRfts pos ts acc
        makeRfts pos [] acc = return $ map (\(i,t) -> RecordFieldType (Identifier i pos) t) acc

isRecordType :: Type -> TI Bool
isRecordType (TVar (Identifier id _)) = do
    state <- get
    return $ any (\(i:>:_) -> i == id) $ recordStack state

tiLit :: Literal -> TI Type
tiLit (Char _) = return tChar
tiLit (String _) = return tString
tiLit (Integer _) = return tNum
tiLit (Float _) = return tNum
tiLit UnitValue = return tUnit

tiExp :: [Assumption] -> Expression -> TI Type
tiExp as (EVariable id) = trace ("lookup "++show id) $ find id $ trace ("database "++show as) as
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
    argsBasedType <- funOf pos tes >>= simplify -- >>= anonimize pos
    unify pos (trace (show argsBasedType) argsBasedType) (trace (show stap) stap)
    astap <- applySubst stap
    return $ funResult astap (length es)
tiExp as (ERecordField pos e id) = do
    t <- tiExp as e >>= simplify -- >>= anonimize pos
    case t of
        TRecord rfts -> do
            trace ("Field "++show id++" of record "++show t) return ()
            case recField rfts id of
                    Just t' -> return t'
                    Nothing -> 
                        case id of
                            Identifier idd _ -> posFail pos $ "Record has no field `"++idd++"`"
        TVar{} -> do
            rt <- newTVar pos
            pushRecordField t (TRecord [RecordFieldType id rt])
            return rt
        _ -> posFail pos "Not a record type"
    where 
        recField [] _ = Nothing
        recField ((RecordFieldType (Identifier id pos) t):rfts) iid@(Identifier id' _) | id == id' = Just t
                            | otherwise = recField rfts iid
tiExp as (ELet pos bp elet econt) = do
    (as', t, tvs) <- tiBPat as bp
    t' <- tiExp as' elet
    unify pos t' t
    materializeRecords pos tvs
    as'' <- applySubst as'
    tiExp as'' econt
tiExp as (EDo pos edo econt) = do
    t <- tiExp as edo
    unify pos t tUnit
    tiExp as econt
tiExp as (ETuple pos es) = TTuple <$> mapM (tiExp as) es
tiExp as (EList pos es) =
    case es of
        [] ->
            do 
                n <- newTVar pos
                simplify (TList pos n)
        _ -> do
                ts <- mapM (tiExp as) es
                foldM_ (\l r -> do unify pos l r; return r) (head ts) (tail ts)
                simplify (TList pos $ head ts) -- >>= anonimize pos
tiExp as (EIf pos eb et ef) = do
    tb <- tiExp as eb >>= simplify
    unify pos tb tBool
    tt <- tiExp as et >>= simplify -- >>= anonimize pos
    tf <- tiExp as ef >>= simplify -- >>= anonimize pos
    unify pos tt tf
    return tt
tiExp as (EIfDo pos eb edo econt) = do
    tb <- tiExp as eb >>= simplify
    unify pos tb tBool
    td <- tiExp as edo >>= simplify
    unify pos td tUnit
    tiExp as econt >>= simplify -- >>= anonimize pos
tiExp as (ELambda pos ps e) = do
    as' <- foldM (\as_ p -> case p of
                                Parameter (Identifier i pos') -> do
                                    n <- newTVar pos'
                                    return $ (i :>: n) : as_
                                _ -> return as_) as ps
    argT <- mapM (parType as') ps
    materializeRecords pos argT
    t <- tiExp as' e
    return $ makeFun pos argT t
tiExp as (EListSequence pos efrom eto) = do
    tfrom <- tiExp as efrom
    unify pos tfrom tNum
    tto <- tiExp as eto
    unify pos tto tNum
    return $ TList pos tNum
tiExp as (ERecord pos rfas) = do
    rfts <- mapM (\(RecordFieldAssignment pos' id e) -> do t <- tiExp as e; return $ RecordFieldType id t) rfas
    return $ TRecord rfts
-- TODO EListComprehension
-- TODO ERecordUpdate
-- TODO EMatch

materializeRecords pos = mapM_ (\id -> do chk <- isRecordType id; if chk then do t <- popRecordField id; unify pos t id else return ())

parType :: [Assumption] -> Param -> TI Type
parType as (Parameter id) = find id as
parType as (Unit _) = return tUnit 
parType as (WildCard pos) = newTVar pos

makeFun :: SourcePos -> [Type] -> Type -> Type
makeFun pos [] t = t
makeFun pos (h:t') t = TFunction pos h $ makeFun pos t' t

tiBPat :: [Assumption] -> BindPattern -> TI ([Assumption], Type, [Type])
tiBPat as (BParenthesis bp) = tiBPat as bp
tiBPat as (BOp pos op ps) = 
    case ps of
        [] -> tiBPat as (BVariable (opId op pos))
        _ -> tiBPat as (BFunctionDecl (opId op pos) ps)
tiBPat as (BWildCard pos) = do n <- newTVar pos; return (as, n, [])
tiBPat as (BVariable (Identifier id pos)) = do
    n <- newTVar pos
    return ((id :>: n):as, n, [n])
tiBPat as (BList pos bps) = do
    (as', ts, tvs) <- foldM (\(as'', ts', tvs') bp -> do (ass, t, tvv) <- tiBPat as'' bp; return (ass, t:ts', tvv ++ tvs')) (as, [], []) bps
    foldM_ (\l r -> do unify pos l r; return r) (head ts) (tail ts)
    let tss = reverse ts
    return (as', TList pos $ head tss, tvs)
tiBPat as (BTuple pos bps) = do
    (as', ts, tvs) <- foldM (\(as'', ts', tvs') bp -> do (ass, t, tvv) <- tiBPat as'' bp; return (ass, t:ts', tvv ++ tvs')) (as, [], []) bps
    return (as', TTuple $ reverse ts, tvs)
tiBPat as (BListHead pos bphead bptail) = do
    (ash, th, tvh) <- tiBPat as bphead
    (ast, tt, tvt) <- tiBPat ash bptail
    sth <- simplify th
    stt <- simplify tt
    unify pos stt (TList pos sth)
    return (ast, stt, tvh ++ tvt)
tiBPat as (BRecord pos rbps) = do
    (as', rfts_, tvs) <- foldM (\(as'', rfts', tvs') (RecordBindPattern pos id bp) -> do (ass, t, tvv) <- tiBPat as'' bp; return (ass, (id,t):rfts', tvv ++ tvs'))  (as, [], []) rbps
    let rfts = map (\(id, t) -> RecordFieldType id t) rfts_
    return (as', TRecord rfts, tvs)
tiBPat as (BFunctionDecl (Identifier id pos) ps) = do
    fn <- newTVar pos
    as' <- foldM (\as_ p -> case p of
                                Parameter (Identifier i pos') -> do
                                    n <- newTVar pos'
                                    return $ (i :>: n) : as_
                                _ -> return as_) as ps
    let as'' = (id :>: fn) : as'
    t <- newTVar pos
    argT <- mapM (parType as'') ps
    trace ("Bind Function `"++id++"` of "++show (makeFun pos argT t)) return ()
    unify pos fn $ makeFun pos argT t
    return (as'', t, argT)

anonimize :: SourcePos -> Type -> TI Type
anonimize pos t@(TVar _) = return t
anonimize pos t = do
    -- let tv = typeVariables t
    -- mapM_ (\id -> newTVar pos >>= \n -> unify pos n (TVar $ ident id)) tv
    -- applySubst t
    ["" :>: t'] <- deepAnonimize ["" :>: t]
    trace ("Anonimize <<"++show t++">>  {{"++show t'++"}}") return t'

deepAnonimize :: [Assumption] -> TI [Assumption]
deepAnonimize = mapM anonA
    where
        anonA (id :>: t) = clearSubst $ do 
            t' <- simplify t
            t'' <- anonT t'
            return $ id :>: t''
        anonT a@(TVar (Identifier id pos)) = do
            s <- subst
            case lookup id s of
                Nothing -> do
                    n <- newTVar pos
                    unify pos a n
                    applySubst n
                Just v -> return v
        anonT (TAlias id t) = anonT t >>= return . TAlias id
        anonT (TList pos t) = anonT t >>= return . TList pos
        anonT (TTuple ts) = mapM anonT ts >>= return . TTuple
        anonT (TFunction pos t1 t2) = do
            t1' <- anonT t1
            t2' <- anonT t2
            return $ TFunction pos t1' t2'
        anonT (TRecord rfts) = mapM (\(RecordFieldType id t) -> do t' <- anonT t; return $ RecordFieldType id t') rfts >>= return . TRecord
        anonT t = return t

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

prepValDecl :: ([Assumption], [(Expression, Type, [Type], SourcePos)]) -> ValueDeclaration -> TI ([Assumption], [(Expression, Type, [Type], SourcePos)])
prepValDecl (as, etts) (Val pos bp e) = do
    (as', t, tvs) <- tiBPat as bp
    return (as', (e, t, tvs, pos):etts)

tiValDecls :: [Assumption] -> [Assumption] -> [ValueDeclaration] -> TI [Assumption]
tiValDecls annotations as ds = do
    anonAs <- deepAnonimize as
    (as', etts) <- foldM prepValDecl (anonAs,[]) ds
    as'' <- foldM (\ass (e, t, tvs, p) -> do
                            t' <- tiExp ass e
                            unify p t' t
                            materializeRecords p tvs
                            applySubst ass) as' etts
    -- ts <- mapM (\(Val (Identifier _ pos) _) -> newTVar pos) ds
    -- let ids = map (\(Val (Identifier id _) _) -> id) ds
    --     idPos = map (\(Val (Identifier id pos) _) -> (id, pos)) ds
    --     as' = zipWith (:>:) ids ts ++ anonAs
    --     exps = map (\(Val _ e) -> e) ds
    -- zipWithM_ (tiUnifyExp as') (zip exps ts) (map snd idPos)
    -- s <- subst
    -- let ts' = apply s ts
    --     as'' = zipWith (:>:) ids ts'
    mapM_ (\(id :>: t) -> case findMaybe id annotations of
                            Nothing -> return ()
                            Just t' -> do
                                let p = baseLibPos --TODO posOfType t'
                                unify p t t'
                                exact p t t') as''
    return as''

testValDecl = [
    Val baseLibPos (BVariable $ ident "x") 
        (ENegative baseLibPos $ EVariable (ident "y")),
    Val baseLibPos (BVariable $ ident "y") (ELiteral baseLibPos $ Integer 1),
    Val baseLibPos (BVariable $ ident "z") 
        (EOp baseLibPos (EVariable $ ident "x") (Op "+") (EVariable $ ident "y")),
    Val baseLibPos (BVariable $ ident "h") 
        (EApplication baseLibPos (EVariable $ ident "f") [ELiteral baseLibPos $ Char 'a', ELiteral baseLibPos $ Integer 1, ELiteral baseLibPos $ String "str"]),
    -- Val (ident "mid") 
    --     (EApplication baseLibPos (EVariable (ident "id")) [EVariable (ident "id")]),
    Val baseLibPos (BVariable $ ident "one") 
        (EApplication baseLibPos (EVariable (ident "id")) [ELiteral baseLibPos $ Integer 1])
    -- Val baseLibPos (BVariable $ ident "loop")
    --     (ELambda baseLibPos [Parameter $ ident "l"] 
    --         (EApplication baseLibPos
    --             (EVariable $ ident "l")
    --             [EVariable $ ident "l"]))
    -- Val baseLibPos (BVariable $ ident "loopRun")
    --     (EApplication baseLibPos
    --         (EVariable $ ident "loop")
    --         [EVariable $ ident "loop"])
    ]

testAddOp = "(+)" :>: TFunction baseLibPos tNum (TFunction baseLibPos tNum tNum)
testFunction = "f" :>: TFunction baseLibPos tChar (TFunction baseLibPos tNum (TFunction baseLibPos tString tNum))
testId = "id" :>: TFunction baseLibPos (TVar $ ident "a") (TVar $ ident "a")
ignore = "ignore" :>: TFunction baseLibPos (TVar $ ident "a") tUnit

boolVal = ["True" :>: tBool, "False" :>: tBool]