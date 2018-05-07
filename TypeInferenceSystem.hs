module TypeInferenceSystem where

import AST

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List ((\\), union, nub, intersect)
import Data.Maybe
import Data.Char (isUpper)

import Debug.Trace

import Text.Parsec.Pos (SourcePos, initialPos)

-- THIS SHOULD BE IN THE transformers LIBRARY
instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

baseLibPos = initialPos "base"

ident :: Ident -> Identifier
ident i = Identifier i baseLibPos

opId :: Op -> SourcePos -> Identifier
opId op = Identifier $ opIdent op

opIdent :: Op -> Ident
opIdent (Op op) = "("++op++")"

-- BUILTIN TYPES
tUnit = TNamed $ ident "()"
tChar = TNamed $ ident "Char"
tNum = TNamed $ ident "Number"
tBool = TUnion (ident "Bool") [UDEnum (ident "True"), UDEnum (ident "False")]
-- tList = TConstr [ident "a"] (TList baseLibPos (TVar $ ident "a"))
-- tFun = TConstr [(ident "a"),(ident "b")] (TFunction baseLibPos (TVar $ ident "a") (TVar $ ident "b"))

tString = TAlias (ident "String") $ list tChar

fun t1 t2 = TFunction baseLibPos t1 t2
list t = TList baseLibPos t
var x = TVar $ ident x

baseLibTypes = [tUnit, tChar, tNum, tBool, tString]

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

simplify :: Type -> TI Type
simplify (TApp pos (TConstr ids tc) ts) | length ids == length ts = do
                                            t <- simplify tc
                                            tss <- mapM simplify ts
                                            return $ zip (uids ids) tss `apply` t
                                        | otherwise = posFail pos "Incorrect number of type parameters"
simplify tt@(TApp pos t@(TNamed (Identifier i _)) ps) =
    clearViewedTypes $ do
        t' <- viewType t i
        if t' == t then return tt
        else simplify $ TApp pos t' ps
simplify (TApp pos t@(TAlias{}) ts) = do t' <- simplify t; simplify (TApp pos t' ts)
simplify t@(TApp pos _ _) = posFail pos $ "Not a type constructor: "++show t
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
simplify (TUnion id uds) = return . TUnion id =<< mapM (\ud -> case ud of
                                                                UDEnum{} -> return ud
                                                                UDTyped id t -> do
                                                                    t' <- simplify t
                                                                    return $ UDTyped id t') uds
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

posOfType (TFunction p _ _) = p
posOfType (TTuple (t:ts)) = posOfType t
posOfType (TList p _) = p
posOfType (TUnit p) = p
posOfType (TNamed (Identifier _ p)) = p
posOfType (TAlias (Identifier _ p) _) = p
posOfType (TRecord ((RecordFieldType (Identifier _ p) _):rs)) = p
posOfType (TParenthesis t) = posOfType t
posOfType (TUnion (Identifier _ p) _) = p
posOfType (TVar (Identifier _ p)) = p
posOfType (TConstr _ t) = posOfType t
posOfType (TApp p _ _) = p

-- Most general unifier
infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
s1 @@ s2 = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

merge :: Monad m => Substitution -> Substitution -> m Substitution
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
 where agree = all (\v -> apply s1 (TVar $ ident v) == apply s2 (TVar $ ident v))
                   (map fst s1 `intersect` map fst s2)

mergeApply :: SourcePos -> Substitution -> [(Type, Type)] -> TI Substitution
mergeApply pos l [] = return l
mergeApply pos l ((t, t'):ts) = do
    s <- mgu pos (apply l t) (apply l t')
    let l' = s @@ l
    mergeApply pos l' ts

mgu :: SourcePos -> Type -> Type -> TI Substitution
mgu pos (TAlias _ t) t' = mgu pos t t'
mgu pos t (TAlias _ t') = mgu pos t t'

mgu pos (TFunction _ t1 t2) (TFunction _ t1' t2') =
    mergeApply pos nullSubst $ zip [t1,t2] [t1', t2']
mgu pos t1@(TTuple ts) t2@(TTuple ts') =
    if length ts' > length ts then posFail pos $ expected t2 actual t1 "Tuple with not enough elements" else
    mergeApply pos nullSubst (zip ts ts')

mgu pos (TList _ t) (TList _ t') = mgu pos t t'

mgu pos (TUnion id uds) (TUnion id' uds') | id == id' = mergeApply pos nullSubst $ zip (types uds) (types uds')
    where
        types uds = tps uds []
        tps [] acc = reverse acc
        tps (UDEnum _ : t) acc = tps t acc
        tps (UDTyped _ t : ts) acc = tps ts (t:acc)

mgu pos (TRecord rfts) (TRecord rftsExpected) = do
    let pairsM = map (\(id, t) -> (id, t, lookup id $ map recFType rfts)) $ map recFType rftsExpected
    mapM_ (\(id, t, mb) -> case mb of
                            Nothing -> posFail pos $ "Record doesn't have field `"++id++"`"
                            _ -> return () ) pairsM
    let pairsF = filter (\(id,t, tm) -> isJust tm) pairsM
    let pairs = map (\(id, t, Just t') -> (t,t')) pairsF
    mergeApply pos nullSubst pairs
    where
        recFType (RecordFieldType (Identifier id _) t) = (id, t)

mgu pos t1@(TRecord rfts) t2@(TVar id) = pushRecordField t2 t1 >> return nullSubst
mgu pos t1@(TVar id) t2@(TRecord rfts) = pushRecordField t1 t2 >> return nullSubst

mgu pos (TVar (Identifier u _)) t = varBind pos u t
mgu pos t (TVar (Identifier u _)) = varBind pos u t

mgu pos (TUnit _) (TUnit _) = return nullSubst
mgu pos (TNamed (Identifier i _)) (TNamed (Identifier i' _)) | i == i' = return nullSubst

mgu pos (TNamed (Identifier i _)) (TApp _ (TNamed (Identifier i' _)) ts) | i == i' = return nullSubst
mgu pos (TApp _ t ts) (TApp _ t' ts') = do
    s <- mgu pos t t'
    mergeApply pos s (zip ts ts')

mgu pos t1 t2@(TNamed (Identifier i _)) =
    clearViewedTypes $ do
        t <- viewType t2 i
        if t == t2 then posFail pos $ expected t2 actual t1 "Types do not unify"
        else mgu pos t1 t

mgu pos t1@(TNamed (Identifier i _)) t2 =
    clearViewedTypes $ do
        t <- viewType t1 i
        if t == t1 then posFail pos $ expected t2 actual t1 "Types do not unify"
        else mgu pos t t2

mgu pos (TConstr _ t) t2 = mgu pos t t2
mgu pos t1 (TConstr _ t) = mgu pos t1 t

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
        recordStack :: [Assumption],
        types_ :: [(Ident, Type)],
        viewTypes_ :: [Ident]
    } deriving (Eq, Show)
initialInferenceState = InferenceState {subst_ = nullSubst, counter = 0, recordStack = [], types_ = [], viewTypes_ = []}

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
    t1' <- simplify t1
    t2' <- simplify t2
    u <- mgu pos (apply s t1') (apply s t2')
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

posWarn :: (MonadTrans f) => SourcePos -> String -> f IO ()
posWarn pos msg = lift $ putStrLn ("WARNING\t"++show pos ++ "\n  "++msg)

expected :: (Show t) => t -> a -> t -> String -> String
expected tex _ tact msg = msg ++ "\n   Expected: "++show tex++"\n   Actual:   "++show tact

actual :: ()
actual = ()

applySubst :: Types t => t -> TI t
applySubst t = do
    s <- subst
    return $ apply s t

pushRecordField :: Type -> Type -> TI ()
pushRecordField (TVar (Identifier id _)) fieldType = do
    state <- get
    put $ state{recordStack = (id :>: fieldType) : recordStack state}

popRecordField :: Type -> TI Type
popRecordField (TVar (Identifier id pos)) = do
    state <- get
    let fieldTypes = filter (\(i:>:_) -> i == id) $ recordStack state
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
        makeRfts pos ((id :>: TRecord rfts):ts) acc = makeRfts pos ((map (\rft -> id :>: TRecord [rft]) rfts)++ts) acc
        makeRfts pos [] acc = return $ map (\(i,t) -> RecordFieldType (Identifier i pos) t) acc

isRecordType :: Type -> TI Bool
isRecordType (TVar (Identifier id _)) = do
    state <- get
    return $ any (\(i:>:_) -> i == id) $ recordStack state
isRecordType _ = return False

setTypesDictionary :: [(Ident, Type)] -> TI ()
setTypesDictionary ts = do
    state <- get
    put $ state {types_ = ts}

types :: TI [(Ident, Type)]
types = types_ <$> get

viewType :: Type -> Ident -> TI Type
viewType t id = do
    return ()
    ts <- types
    state <- get
    if elem id $ viewTypes_ state then return t
    else do
        put $ state { viewTypes_ = id : viewTypes_ state }
        case lookup id ts of
            Nothing -> return t
            Just tt -> 
                case tt of
                    TUnion{} -> return t
                    TConstr _ (TUnion{}) -> return t
                    _ -> return tt

clearViewedTypes :: TI a -> TI a
clearViewedTypes action = do
    state <- get
    let viewed = viewTypes_ state
    r <- action
    state' <- get
    put $ state' {viewTypes_ = viewed}
    return r 

view :: Type -> (Type -> TI Type) -> TI Type
view t f =
    case t of
        TNamed (Identifier id _) ->
            clearViewedTypes $ do
                t' <- viewType t id
                if t == t' then f t
                else case t' of
                        TAlias _ tt -> f tt
                        _ -> f t'
        _ -> f t 

tiLit :: Literal -> TI Type
tiLit (Char _) = return tChar
tiLit (String _) = return tString
tiLit (Integer _) = return tNum
tiLit (Float _) = return tNum
tiLit UnitValue = return tUnit

tiExp :: [Assumption] -> Expression -> TI Type
tiExp as (EVariable id@(Identifier iid pos)) = do
    t <- find id as
    case t of
        TVar{} -> return t
        _ -> anonimize pos t
tiExp as (ELiteral pos lit) = tiLit lit
tiExp as (EParenthesis e) = tiExp as e
tiExp as (ENegative pos e) = do
    t <- tiExp as e
    unify pos t tNum
    return t
tiExp as (EOp pos el op er) = tiExp as (EApplication pos (EVariable $ opId op pos) [el, er])
tiExp as (ETyped pos e t) = do
    t' <- tiExp as e
    ta <- anonimize pos t
    unify pos t' ta
    return ta
tiExp as (EApplication pos eapp es) = do
    stap <- tiExp as eapp >>= simplify >>= anonimize pos
    tes <- mapM (tiExp as) es
    argsBasedType <- funOf pos tes >>= simplify
    unify pos argsBasedType stap
    astap <- applySubst stap
    return $ funResult astap (length es)
tiExp as (ERecordField pos e id) = do
    tt <- tiExp as e >>= simplify
    view tt $ \t ->
        case t of
            TRecord rfts -> do
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
    (asG, asL, t, tvs) <- tiBPat bp
    t' <- tiExp (asG ++ asL ++ as) elet
    unify pos t' t
    materializeRecords pos tvs
    as'' <- applySubst (asG ++ as)
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
                simplify (TList pos $ head ts)
tiExp as (EIf pos eb et ef) = do
    tb <- tiExp as eb >>= simplify
    unify pos tb tBool
    tt <- tiExp as et >>= simplify
    tf <- tiExp as ef >>= simplify
    unify pos tt tf
    return tt
tiExp as (EIfDo pos eb edo econt) = do
    tb <- tiExp as eb >>= simplify
    unify pos tb tBool
    td <- tiExp as edo >>= simplify
    unify pos td tUnit
    tiExp as econt >>= simplify
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
tiExp as (EMatch pos e alts) = do
    t <- tiExp as e
    (pt, et) <- foldM (\(pt', et') alt -> do (ptt, ett) <- tiAlt as alt; return (ptt:pt', ett:et')) ([],[]) alts
    foldM_ (\t1 t2 -> do unify pos t1 t2; return t2) (head et) (tail et)
    mapM_ (\t' -> do unify pos t t') pt
    materializeRecords pos [t]
    return $ head et
tiExp as (ERecordUpdate pos id rfas) = do
    tt <- tiExp as (EVariable id)
    view tt $ \t ->
        case t of
            TRecord rftsL -> do
                rftsR <- mapM (\(RecordFieldAssignment pos' id e) -> do t <- tiExp as e; return $ RecordFieldType id t) rfas
                let tout = TRecord $ mergeRecs rftsL rftsR
                unify pos tout t
                return $ tout
            TVar{} -> do
                n <- newTVar pos
                rftsR <- mapM (\(RecordFieldAssignment pos' id e) -> do t <- tiExp as e; return $ RecordFieldType id t) rfas
                mapM_ (\rft -> pushRecordField n $ TRecord [rft]) rftsR
                return n
            _ -> posFail pos $ "Cannot extend a non record value\n   Actual: " ++ show t
    where
        mergeRecs rs rs' = foldl (\acc r@(RecordFieldType id t) -> if contains rs id then acc else r:acc) rs rs'
        contains ((RecordFieldType (Identifier id _) _):t) i@(Identifier id' _) =
            id == id' || contains t i
        contains [] _ = False

tiExp as (EListComprehension pos e comps) = do
    as' <- tiComps as comps
    t <- tiExp as' e
    return $ TList pos t

materializeRecords pos = mapM_ (\id -> do chk <- isRecordType id; if chk then do t <- popRecordField id; unify pos t id else return ())

parType :: [Assumption] -> Param -> TI Type
parType as (Parameter id) = find id as
parType as (Unit _) = return tUnit 
parType as (WildCard pos) = newTVar pos

makeFun :: SourcePos -> [Type] -> Type -> Type
makeFun pos [] t = t
makeFun pos (h:t') t = TFunction pos h $ makeFun pos t' t

tiComps as comps = foldM (\as' c -> tiComp as' c) as comps
tiComp as (Comprehension pos bp exp) = do
    (asG, asL, t, tvs) <- tiBPat bp
    t' <- tiExp (asG ++ asL ++ as) exp
    case t' of
        TList _ t'' -> do
            unify pos t'' t
            materializeRecords pos tvs
            applySubst (asG ++ as)
        TVar{} -> do
            n <- newTVar pos
            unify pos t' (TList pos n)
            materializeRecords pos tvs
            applySubst (asG ++ as)
        _ -> posFail pos "Expression in the comprehension must be a list"


tiAlt :: [Assumption] -> Alternate -> TI (Type, Type)
tiAlt as (Alternate pos pat e) = do
    (as', pt, tvs) <- tiPat as pat
    et <- tiExp as' e
    materializeRecords pos tvs
    return (pt, et)

tiPat :: [Assumption] -> Pattern -> TI ([Assumption], Type, [Type])
tiPat as (PParenthesis p) = tiPat as p
tiPat as (PVariable i@(Identifier id pos)) =
    let v = findMaybe id as in
    if isJust v && isUpper (head id) then
        return (as, fromJust v, [])
    else do
        n <- newTVar pos
        return ((id:>:n):as, n, [n])
tiPat as (PLiteral pos lit) = do
    t <- tiLit lit
    return (as, t, [])
tiPat as (PApplication pos i@(Identifier id _) pat) = do
    at <- find i as >>= anonimize pos
    case at of
        TFunction pos' t1 t2 -> do
            (as', t1', tvs) <- tiPat as pat
            unify pos' t1' t1
            return (as', t2, tvs)
        _ -> posFail pos $ "`"++id++"` is not a applicative constructor"
tiPat as (PTuple pos ps) = do
    (as', ts, tvs) <- foldM (\(as'', ts', tvs') p -> do (ass, t, tvv) <- tiPat as'' p; return (ass, t:ts', tvv ++ tvs')) (as, [], []) ps
    return (as', TTuple $ reverse ts, tvs)
tiPat as (PList pos ps) = do
    case ps of
        [] -> do
            n <- newTVar pos
            return (as, TList pos n, [])
        _ -> do
            (as', ts, tvs) <- foldM (\(as'', ts', tvs') p -> do (ass, t, tvv) <- tiPat as'' p; return (ass, t:ts', tvv ++ tvs')) (as, [], []) ps
            foldM_ (\l r -> do unify pos l r; return r) (head ts) (tail ts)
            return (as', TList pos $ head ts, tvs)
tiPat as (PListHead pos phead ptail) = do
    (ash, th, tvh) <- tiPat as phead
    (ast, tt, tvt) <- tiPat ash ptail
    sth <- simplify th
    stt <- simplify tt
    unify pos stt (TList pos sth)
    return (ast, stt, tvh ++ tvt)
tiPat as (PListContains pos e) = do
    t <- tiExp as e
    return (as, TList pos t, [])
tiPat as (PWildCard pos) = do
    n <- newTVar pos
    return (as, n, [])
tiPat as (PRecord pos rps) = do
    (as', ts, tvs) <- foldM (\(as'', ts', tvs') (RecordPattern _ _ p) -> do (ass, t, tvv) <- tiPat as'' p; return (ass, t:ts', tvv ++ tvs')) (as, [], []) rps
    let t = TRecord $ map (\(t, RecordPattern _ id _) -> RecordFieldType id t) (zip (reverse ts) rps)
    return $ (as', t, tvs)
    

tiBPat :: BindPattern -> TI ([Assumption], [Assumption], Type, [Type])
tiBPat (BParenthesis bp) = tiBPat bp
tiBPat (BOp pos op ps) = 
    case ps of
        [] -> tiBPat (BVariable (opId op pos))
        _ -> tiBPat (BFunctionDecl (opId op pos) ps)
tiBPat (BWildCard pos) = do n <- newTVar pos; return ([], [], n, [])
tiBPat (BVariable (Identifier id pos)) = do
    n <- newTVar pos
    return ([id :>: n], [], n, [n])
tiBPat (BList pos bps) = do
    (as', ts, tvs) <- foldM (\(as'', ts', tvs') bp -> do (ass, _, t, tvv) <- tiBPat bp; return (ass++as'', t:ts', tvv ++ tvs')) ([], [], []) bps
    foldM_ (\l r -> do unify pos l r; return r) (head ts) (tail ts)
    let tss = reverse ts
    return (as', [], TList pos $ head tss, tvs)
tiBPat (BTuple pos bps) = do
    (as', ts, tvs) <- foldM (\(as'', ts', tvs') bp -> do (ass, _, t, tvv) <- tiBPat bp; return (ass++as'', t:ts', tvv ++ tvs')) ([], [], []) bps
    return (as', [], TTuple $ reverse ts, tvs)
tiBPat (BListHead pos bphead bptail) = do
    (ash, _, th, tvh) <- tiBPat bphead
    (ast, _, tt, tvt) <- tiBPat bptail
    sth <- simplify th
    stt <- simplify tt
    unify pos stt (TList pos sth)
    return (ast ++ ash, [], stt, tvh ++ tvt)
tiBPat (BRecord pos rbps) = do
    (as', rfts_, tvs) <- foldM (\(as'', rfts', tvs') (RecordBindPattern pos id bp) -> do (ass, _, t, tvv) <- tiBPat bp; return (ass, (id,t):rfts', tvv ++ tvs'))  ([], [], []) rbps
    let rfts = map (\(id, t) -> RecordFieldType id t) rfts_
    return (as', [], TRecord rfts, tvs)
tiBPat (BFunctionDecl (Identifier id pos) ps) = do
    fn <- newTVar pos
    as' <- foldM (\as_ p -> case p of
                                Parameter (Identifier i pos') -> do
                                    n <- newTVar pos'
                                    return $ (i :>: n) : as_
                                _ -> return as_) [] ps
    t <- newTVar pos
    argT <- mapM (parType ((id:>:fn):as')) ps
    unify pos fn $ makeFun pos argT t
    return ([id :>: fn], as', t, argT)

anonimize :: SourcePos -> Type -> TI Type
anonimize pos t@(TVar _) = return t
anonimize pos t = do
    -- let tv = typeVariables t
    -- mapM_ (\id -> newTVar pos >>= \n -> unify pos n (TVar $ ident id)) tv
    -- applySubst t
    ["" :>: t'] <- deepAnonimize ["" :>: t]
    return t'

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
        anonT (TApp pos t ts) = do
            t' <- anonT t
            ts' <- mapM anonT ts
            return $ TApp pos t' ts'
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

prepValDecl (as, etts) (Val pos bp e) = do
    (asG, asL, t, tvs) <- tiBPat bp
    return (as++asG, (asG, asL, e, t, tvs, pos):etts)

tiValDecls :: [Assumption] -> [Assumption] -> [ValueDeclaration] -> TI [Assumption]
tiValDecls annotations as ds = do
    anonAs <- deepAnonimize as
    (as', etts) <- foldM prepValDecl (anonAs,[]) ds
    (as'', das) <- foldM (\(ass, das) (asG, asL, e, t, tvs, p) -> do
                            t' <- tiExp (asL++ass) e
                            unify p t' t
                            materializeRecords p tvs
                            ass' <- applySubst ass
                            asG' <- applySubst asG
                            return (ass', asG' ++ das)) (as', []) etts
    mapM_ (\(id :>: t) -> case findMaybe id annotations of
                            Nothing -> return ()
                            Just t' -> do
                                let p = posOfType t'
                                t'' <- anonimize p t'
                                unify p t t''
                                ) das
    das' <- applySubst das
    return das'

baseLibAssumptions = [
    "(+)" :>: fun tNum (fun tNum tNum),
    "(-)" :>: fun tNum (fun tNum tNum),
    "(*)" :>: fun tNum (fun tNum tNum),
    "(/)" :>: fun tNum (fun tNum tNum),
    "(**)" :>: fun tNum (fun tNum tNum),
    "(%)" :>: fun tNum (fun tNum tNum),
    "(&&)" :>: fun tBool (fun tBool tBool),
    "(||)" :>: fun tBool (fun tBool tBool),
    "(==)" :>: fun (var "a") (fun (var "a") tBool),
    "(<=)" :>: fun (var "a") (fun (var "a") tBool),
    "(>=)" :>: fun (var "a") (fun (var "a") tBool),
    "(++)" :>: fun (list $ var "a") (fun (list $ var "a") (list $ var "a")),
    "(:)" :>: fun (var "a") (fun (list $ var "a") (list $ var "a")),
    "id" :>: fun (var "a") (var "a"),
    "ignore" :>: fun (var "a") tUnit,
    "print" :>: fun (var "a") tUnit,
    "printStr" :>: fun tString tUnit,
    "True" :>: tBool, "False" :>: tBool,
    "die" :>: fun tString (var "a"),
    "head" :>: fun (list (var "a")) (var "a"),
    "tail" :>: fun (list (var "a")) (list (var "a")),
    "(!!)" :>: fun (list (var "a")) (fun tNum (var "a")),
    "toString" :>: fun (var "a") tString,
    "toNum" :>: fun tString tNum,
    "readln" :>: fun tUnit tString,
    "map" :>: fun (fun (var "a") (var "b")) (fun (list (var "a")) (list (var "b"))),
    "flatten" :>: fun (list $ list $ var "a") (list $ var "a"),
    
    -- INTERNAL DEBUG RELATED FUNCTIONS
    "__dump_store" :>: fun tUnit tUnit
 ]

mainAnn = "main" :>: fun (list tString) tNum
 