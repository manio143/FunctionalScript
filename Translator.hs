module Translator where

import AST
import ProgramState

import TypeInferenceSystem

import Text.Parsec.Pos

import Debug.Trace

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad

import Data.List
import Data.Char
import Data.Either

-- THIS SHOULD BE IN THE transformers LIBRARY
instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

translate :: AST.Program -> IO Store
translate (Program decls) = evalStateT inner initialInferenceState
    where
        inner :: TI Store
        inner = do
        let (types, annotations, assumptions, valDecls) = divideDecls decls
        (types', annotations', assumptions', valDecls') <- checkTypes (types ++ map Right baseLibTypes) annotations assumptions valDecls
        lift $ print types'
        lift $ print annotations'
        lift $ print assumptions'
        lift $ print valDecls'
        let valDs = divideDependant valDecls'
        as <- typecheck valDs annotations' assumptions'
        lift $ print as
        let st = constructorStore assumptions baseLibStore
        foldM valDeclToStore st valDs

typecheck :: [[ValueDeclaration]] -> [Assumption] -> [Assumption] -> TI [Assumption]
typecheck valDs annotations as = foldM (\as' vd -> do das <- tiValDecls annotations as' vd; return (das++as')) (as ++ baseLibAssumptions) valDs

checkTypes :: (Monad m) => [Either (Identifier, Type) Type] -> [Assumption] -> [Assumption] -> [ValueDeclaration] -> m ([Type], [Assumption], [Assumption], [ValueDeclaration])
-- make sure that for each tvar in angles there is a TVar in body,
-- make sure no undefined name is used
-- make sure you cannot do a cyclic dependency in an alias or a record
-- DONE change lowerletter TNamed to TVar
-- check for type redefinition
checkTypes types annotations assumptions valDecls = do -- TODO above
        let typeNames = namesOfTypes (map eitherUT types) []
        types' <- mergeLefts types (zip typeNames (map eitherUT types))
        assumptions' <- checkAss typeNames assumptions
        annotations' <- checkAss typeNames annotations
        valDecls' <- checkVals typeNames valDecls
        types'' <- mapM (checkType typeNames) types'
        return (types', annotations', assumptions', valDecls')
    where
        namesOfTypes [] acc = reverse acc
        namesOfTypes (h:t) acc = namesOfTypes t (name h : acc)
        name x = case x of
                    TAlias (Identifier id _) _ -> id
                    TNamed (Identifier id _) -> id
                    TUnion (Identifier id _) _ -> id
                    TConstr _ t -> name t

        eitherUT (Right t) = t
        eitherUT (Left (_, t)) = t

        --TODO mergeLefts should fold to allow extending extended types
        mergeLefts ts idts = mapM (mergeLeft idts) ts
        mergeLeft idts (Right t) = return t
        mergeLeft idts (Left ((Identifier id pos), t)) =
            case lookup id idts of
                Nothing -> posFail pos $ "No such type `"++id++"`"
                Just (TAlias _ (TRecord rfts)) -> 
                    case t of
                        TAlias i (TRecord rftsN) -> return . TAlias i . TRecord =<< mergeRec rfts rftsN
                        TConstr ids (TAlias i (TRecord rftsN)) -> return . TConstr ids . TAlias i . TRecord =<< mergeRec rfts rftsN
                Just (TConstr{}) -> posFail pos "Cannot extend generic records"

        mergeRec rfts rftsN = do
            mapM_ (\(RecordFieldType id@(Identifier _ pos) t) -> if elemRec id rfts then posFail pos "Redefintion of record fields is not allowed" else return ()) rftsN
            return $ rfts ++ rftsN
        elemRec i@(Identifier id _) ((RecordFieldType (Identifier id' _) _):rfts) | id == id' = True
               | otherwise = elemRec i rfts
        elemRec _ [] = False

        checkAss names as = mapM (checkAs names) as
        checkAs names (id :>: t) = do
            t' <- checkType names t
            return (id :>: t')
        
        checkVals names vds = mapM (checkVal names) vds
        checkVal names (Val pos bp e) = checkExp names e >>= return . Val pos bp
        checkExp names (EOp pos e1 op e2) = do
            e1' <- checkExp names e1
            e2' <- checkExp names e2
            return $ EOp pos e1' op e2'
        checkExp names (ERecordField pos e id) = do
            e' <- checkExp names e
            return $ ERecordField pos e' id
        checkExp names (ETyped pos e t) = do
            e' <- checkExp names e
            t' <- checkType names t
            return $ ETyped pos e' t'
        checkExp names (EApplication pos e es) = do
            e' <- checkExp names e
            es' <- mapM (checkExp names) es
            return $ EApplication pos e' es'
        checkExp names (ENegative pos e) = do
            e' <- checkExp names e
            return $ ENegative pos e'
        checkExp names (ELet pos bp e1 e2) = do
            e1' <- checkExp names e1
            e2' <- checkExp names e2
            return $ ELet pos bp e1' e2'
        checkExp names (EDo pos e1 e2) = do
            e1' <- checkExp names e1
            e2' <- checkExp names e2
            return $ EDo pos e1' e2'
        checkExp names (EParenthesis e) = checkExp names e >>= return . EParenthesis
        checkExp names (ETuple pos es) = mapM (checkExp names) es >>= return . ETuple pos
        checkExp names (EListSequence pos e1 e2) = do
            e1' <- checkExp names e1
            e2' <- checkExp names e2
            return $ EListSequence pos e1' e2'
        checkExp names (EListComprehension pos e comps) = do
            e' <- checkExp names e
            comps' <- mapM (\(Comprehension pos bp e)-> checkExp names e >>= return . Comprehension pos bp) comps
            return $ EListComprehension pos e' comps'
        checkExp names (EList pos es) = mapM (checkExp names) es >>= return . EList pos
        checkExp names (ERecord pos rfas) = do
            mapM (\(RecordFieldAssignment pos id e) -> checkExp names e >>= return . RecordFieldAssignment pos id) rfas >>= return . ERecord pos
        checkExp names (ERecordUpdate pos id rfas) = do
            mapM (\(RecordFieldAssignment pos id e) -> checkExp names e >>= return . RecordFieldAssignment pos id) rfas >>= return . ERecordUpdate pos id
        checkExp names (EIf pos e1 e2 e3) = do
            e1' <- checkExp names e1
            e2' <- checkExp names e2
            e3' <- checkExp names e3
            return $ EIf pos e1' e2' e3'
        checkExp names (EIfDo pos e1 e2 e3) = do
            e1' <- checkExp names e1
            e2' <- checkExp names e2
            e3' <- checkExp names e3
            return $ EIfDo pos e1' e2' e3'
        checkExp names (ELambda pos ps e) = checkExp names e >>= return . ELambda pos ps
        checkExp names (EMatch pos e alts) = do
            e' <- checkExp names e
            alts' <- mapM (\(Alternate pos p e) -> checkExp names e >>= return . Alternate pos p) alts
            return $ EMatch pos e' alts'
        checkExp names e = return e

        checkType names (TFunction pos t1 t2) = do
            t1' <- checkType names t1
            t2' <- checkType names t2
            return $ TFunction pos t1' t2'
        checkType names (TTuple ts) = return . TTuple =<< mapM (checkType names) ts
        checkType names (TList pos t) = return . TList pos =<< checkType names t
        checkType names (TNamed i@(Identifier id pos)) =
            if not (elem id names) && isLower (head id) then return $ TVar i
            else if elem id names then return $ TNamed i
                 else posFail pos $ "Undefined type `" ++ id ++ "`"
        checkType names (TAlias id t) = checkType names t >>= return . TAlias id
        checkType names (TRecord rfts) = return . TRecord =<< mapM (\(RecordFieldType id t) -> checkType names t >>= return . RecordFieldType id) rfts
        checkType names (TParenthesis t) = checkType names t >>= return . TParenthesis
        checkType names (TUnion id uds) = do
            uds' <- mapM (\ud -> case ud of
                                    UDEnum id -> return ud
                                    UDTyped id t -> checkType names t >>= return . UDTyped id) uds
            return $ TUnion id uds'
        checkType names (TConstr ids t) = do
            t' <- checkType names t
            let tvs = typeVariables t'
            mapM_ (\tv -> if not $ any (\(Identifier id _) -> id == tv) ids then fail $ "Type variable `"++tv++"` not present in type constructor" else return ()) tvs --TODO add pos of type to fail
            return $ TConstr ids t'
        checkType names (TApp pos t ts) = do
            t' <- checkType names t
            ts' <- mapM (checkType names) ts
            return $ TApp pos t' ts'
        checkType _ t = return t


divideDependant :: [ValueDeclaration] -> [[ValueDeclaration]]
-- group mutually recursive declarations 
-- sort based on dependency
divideDependant vds = map (:[]) (reverse vds) -- TODO implement

divideDecls :: [Declaration] -> ([Either (Identifier, Type) Type], [Assumption], [Assumption], [ValueDeclaration])
divideDecls ds = divide ds ([],[],[],[])
    where
        divide [] tup = tup
        divide (h:tl) (ts, ans, as, vds) = 
            case h of
                TypeDeclaration{} -> divide tl (typeOfDecl h : ts, ans, as, vds)
                DataTypeDeclaration{} -> let (t, ass) = dataDecl h in
                                            divide tl (t:ts, ans, ass++as, vds)
                TypeAnnotation (Identifier id _) t -> divide tl (ts, (id :>: t) : ans, as, vds)
                ValueDeclaration bp e -> let p = posOfBindPattern bp in
                                            divide tl (ts, ans, as, Val p bp e : vds)
        typeOfDecl :: Declaration -> Either (Identifier, Type) Type
        typeOfDecl (TypeDeclaration id tp td) =
            case tp of
                EmptyTypeParam -> 
                    case typeOfTD td [] of
                        Right t -> Right $ TAlias id t
                        Left (id, t) -> Left (id, TAlias id t)
                TypeParam tids -> 
                    let ids = map (\(TypeIdentifier id) -> id) tids in
                    case typeOfTD td ids of
                        Right t -> Right $ TConstr ids $ TAlias id t
                        Left (id, t) -> Left (id, TConstr ids $ TAlias id t)
        typeOfTD :: TypeDefinition -> [Identifier] -> Either (Identifier, Type) Type
        typeOfTD (TDAlias t) vars = Right $ applyVars vars t
        typeOfTD (TDRecord rfts) vars = Right $ applyVars vars $ TRecord rfts
        typeOfTD (TDExtension id rfts) vars = Left $ (id, applyVars vars $ TRecord rfts)

        applyVars vars t@(TNamed i@(Identifier id pos)) | varLookup vars id = TVar i
                                                        | otherwise = t
                where
                    varLookup vars id = any (\(Identifier iid _) -> iid == id) vars
        applyVars vars (TFunction pos t1 t2) = TFunction pos (applyVars vars t1) (applyVars vars t2)
        applyVars vars (TTuple ts) = TTuple $ map (applyVars vars) ts
        applyVars vars (TList pos t) = TList pos $ applyVars vars t
        applyVars vars (TRecord rfts) = TRecord $ map (\(RecordFieldType id t) -> RecordFieldType id (applyVars vars t)) rfts
        applyVars vars (TParenthesis t) = TParenthesis $ applyVars vars t
        applyVars _ t = t

        dataDecl (DataTypeDeclaration id@(Identifier _ pos) tp uds) =
            let (t, t') = case tp of
                        EmptyTypeParam -> (TUnion id uds, TNamed id)
                        TypeParam tids -> 
                            let ids = map (\(TypeIdentifier id) -> id) tids in
                            (TConstr ids $ TUnion id (map (applyUDvars ids) uds), TApp pos (TNamed id) (map TVar ids))
            in (Right t, constructors t t')
            where
                applyUDvars vars u@(UDEnum _) = u
                applyUDvars vars u@(UDTyped id t) = UDTyped id (applyVars vars t)
                constructors :: Type -> Type -> [Assumption]
                constructors (TUnion id uds) t' = map (constr t') uds
                constructors (TConstr ids (TUnion id uds)) t' = map (constr t') uds
                constr t (UDEnum (Identifier id _)) = id :>: t
                constr t (UDTyped (Identifier id p) tf) = id :>: TFunction p tf t



posOfBindPattern (BParenthesis bp) = posOfBindPattern bp
posOfBindPattern (BOp pos _ _) = pos
posOfBindPattern (BRecord pos _) = pos
posOfBindPattern (BTuple pos _) = pos
posOfBindPattern (BList pos _) = pos
posOfBindPattern (BListHead pos _ _) = pos
posOfBindPattern (BWildCard pos) = pos
posOfBindPattern (BVariable (Identifier _ pos)) = pos
posOfBindPattern (BFunctionDecl (Identifier _ pos) _) = pos

valDeclToStore :: Store -> [ValueDeclaration] -> TI Store
-- convert value declaration to a ProgramState.Value / Expression
-- previously sorted vds should remove laziness from values
valDeclToStore s vds = -- TODO implement
    case vds of
        [Val pos bp e] ->
            case bp of
                BFunctionDecl (Identifier id pos) params ->
                    return $ withRec id (makeLambda params (transExp e)) s s
                _ -> do
                    trace ("STATIC EVAL of "++show bp) return ()
                    val <- lift $ eval (transExp e) (trace (show s) s)
                    bindValue val bp s
        _ -> fail "TODO implement mutually recursive values"

makeLambda [] e = e
makeLambda (h:t) e = LambdaExpression (convParam h) (makeLambda t e)
makeLambdaHead (h:t) e s = FunctionValue $ Func s (convParam h) $ makeLambda t e

convParam :: AST.Param -> ProgramState.Parameter
convParam (Parameter (Identifier id _)) = BoundParameter id
convParam (AST.WildCard _) = ProgramState.WildCard
convParam (AST.Unit _) = ProgramState.Unit

transExp :: AST.Expression -> ProgramState.Expression
transExp (EOp _ e1 op e2) =
    ApplicationExpression 
        (ApplicationExpression 
            (VariableExpression $ opIdent op) 
            (transExp e1)) 
        (transExp e2)
transExp (ERecordField _ e (Identifier id _)) =
    RecordFieldExpression (transExp e) id
transExp (ETyped _ e _) = transExp e
transExp (EApplication _ e es) = foldl (\el er -> ApplicationExpression el er) (transExp e) $ map transExp es
transExp (ENegative _ e) = NegativeExpression $ transExp e
transExp (ELet _ bp el ec) = makeLet bp (transExp el) (transExp ec)
transExp (ELiteral _ l) = ValueExpression $ litToValue l
transExp (EVariable (Identifier id _)) = VariableExpression id
transExp (EDo _ ed ec) = DoExpression (transExp ed) (transExp ec)
transExp (EParenthesis e) = transExp e
transExp (ETuple _ es) = TupleConstruction $ map transExp es
transExp (EList _ es) = ListConstruction $ map transExp es
transExp (EListSequence _ ef et) = 
    ApplicationExpression
        (ApplicationExpression
            (VariableExpression "__list_sequence")
            (transExp ef))
        (transExp et)
transExp (EListComprehension pos e comps) =
    let (ce, _) = foldl compF (transExp e, 1) comps in ce
    where
        compF (ep, i) (Comprehension _ bp ec) =
            let id = "__x"++show i in
            (ApplicationExpression
                (ApplicationExpression
                    (VariableExpression "map")
                    (LambdaExpression 
                        (BoundParameter id)
                        (makeLet bp (VariableExpression id) ep)))
                (transExp ec)
             , i+1)
transExp (ERecord _ rfas) = 
    RecordConstruction 
        $ map (\(RecordFieldAssignment _ (Identifier id _) e) -> (id, transExp e)) rfas
transExp (ERecordUpdate _ (Identifier id _) rfas) =
    RecordUpdateExpression id $ RecordConstruction 
        $ map (\(RecordFieldAssignment _ (Identifier idd _) e) -> (idd, transExp e)) rfas
transExp (EIf _ ec et ef) = IfExpression (transExp ec) (transExp et) (transExp ef)
transExp (EIfDo _ ec et ef) = IfDoExpression (transExp ec) (transExp et) (transExp ef)
transExp (ELambda _ params e) = foldl (\prev par -> LambdaExpression (convParam par) prev) (transExp e) (reverse params)
transExp (EMatch _ e alts) = MatchExpression (transExp e) (transAlts alts)

transAlts alts = map transAlt alts ++ [altFailure]
altFailure = \_ _ -> return (ApplicationExpression (VariableExpression "die") (ValueExpression $ stringListToValue "Non-exhaustive pattern matching"))
transAlt (AST.Alternate _ pat e) = \val s -> makeAlt pat val s (transExp e)

makeAlt :: AST.Pattern -> ProgramState.Value -> ProgramState.Store -> ProgramState.Expression -> MaybeT IO ProgramState.Expression
makeAlt (PVariable id@(Identifier i _)) val s e = 
    case getVar i s of
        Nothing -> return $ makeLet (BVariable id) (ValueExpression val) e
        Just vv -> case vv of
                    UnionValue{} -> if val == vv then return e else mzero
                    _ -> return $ makeLet (BVariable id) (ValueExpression val) e
makeAlt (PLiteral _ lit) val _ e = if val == litToValue lit then return e else mzero
makeAlt (PApplication _ (Identifier id _) pat) val s e =
    case val of
        UnionValue (idd, UnionWithValue v) -> if id == idd then makeAlt pat v s e else mzero
makeAlt (PTuple _ pats) val s e =
    case val of
        TupleValue vs -> foldr (\(v,p) epm -> do ep <- epm; makeAlt p v s ep) (lift $ return e) (zip vs pats)
makeAlt (PList _ pats) val s e =
    case val of
        ListValue vs ->
            if length vs < length pats then mzero
            else foldr (\(v,p) epm -> do ep <- epm; makeAlt p v s ep) (lift $ return e) (zip vs pats)
makeAlt (PListHead _ pat1 pat2) val s e =
    case val of
        ListValue [] -> mzero
        ListValue (h:t) -> do
            ep <- makeAlt pat2 (ListValue t) s e
            makeAlt pat1 h s ep
makeAlt (PListContains _ e) val s ec = do
     v <- lift $ eval (transExp e) s
     case val of
        ListValue vs -> if elem v vs then return ec else mzero
makeAlt (PParenthesis p) val s ec = makeAlt p val s ec
makeAlt (PWildCard _) _ _ ec = return ec
makeAlt (PRecord _ rps) val s ec =
    case val of
        RecordValue r -> foldr (\(RecordPattern _ (Identifier id _) p) epm -> do ep <- epm; let (Just v) = lookup id r in makeAlt p v s ep) (lift $ return ec) rps

makeLet (BParenthesis bp) el ec = makeLet bp el ec
makeLet (BWildCard _) el ec = DoExpression el ec
makeLet (BFunctionDecl (Identifier id _) params) el ec =
    LetExpression id
        (foldr (\p e -> LambdaExpression (convParam p) e) el params)
        ec
makeLet (BVariable (Identifier id _)) el ec = LetExpression id el ec
makeLet (BOp pos op params) el ec = makeLet (BFunctionDecl (opId op pos) params) el ec 
makeLet (BListHead _ bph bpt) el ec = 
    LetExpression "__temp" el
        (makeLet bph 
            (ApplicationExpression
                (VariableExpression "head")
                (VariableExpression "__temp"))
            (makeLet bpt
                (ApplicationExpression
                    (VariableExpression "tail")
                    (VariableExpression "__temp"))
                ec))
makeLet (BTuple _ bps) el ec =
    LetExpression "__temp" el $ foldr (\(bp, i) e -> 
            makeLet bp (ApplicationExpression
                        (ApplicationExpression
                            (VariableExpression "(!!)")
                            (VariableExpression "__temp")
                         )
                        (ValueExpression $ NumberValue $ Int i)
                        ) e)
            ec (zip bps [0..])
makeLet (BList _ bps) el ec =
    LetExpression "__temp" el
        $ foldr (\(bp, i) e -> makeLet bp (ApplicationExpression
                                        (ApplicationExpression
                                            (VariableExpression "(!!)")
                                            (VariableExpression "__temp"))
                                        (ValueExpression $ NumberValue $ Int i)) e)
            ec (zip bps [0..])
makeLet (BRecord _ rbps) el ec =
    LetExpression "__temp" el
        $ foldr (\(RecordBindPattern _ (Identifier id _) bp) e ->
                    makeLet bp (RecordFieldExpression (VariableExpression "__temp") id) e)
            ec rbps

litToValue (AST.Char c) = CharacterValue c
litToValue (AST.String s) = ListValue $ map CharacterValue s
litToValue (AST.Integer i) = NumberValue $ Int i
litToValue (AST.Float f) = NumberValue $ ProgramState.Float f
litToValue (AST.UnitValue) = ProgramState.UnitValue

bindValue :: Value -> BindPattern -> Store -> TI Store
bindValue val (BParenthesis bp) s = bindValue val bp s
bindValue val (BOp pos op params) s = return $ withVar (opIdent op) val s
bindValue val (BRecord pos rbps) s = 
    case val of
        RecordValue rs ->
            foldM (\s' (RecordBindPattern pos (Identifier id _) bp) -> 
                        case lookup id rs of
                            Just v -> bindValue v bp s') s rbps
bindValue val (BList pos bps) s =
    case val of
        ListValue l ->
            if length l < length bps then posFail pos "Too few elements in list during static binding"
            else foldM (\s' (v, bp) -> bindValue v bp s') s (zip l bps)
bindValue val (BTuple pos bps) s =
    case val of
        TupleValue l ->
            if length l < length bps then posFail pos "Too few elements in tuple during static binding"
            else foldM (\s' (v, bp) -> bindValue v bp s') s (zip l bps)
bindValue val (BListHead pos bph bpt) s =
    case val of
        ListValue (h:t) -> do
            s' <- bindValue h bph s
            bindValue (ListValue t) bpt s'
bindValue val (BWildCard _) s = return s
bindValue val (BVariable (Identifier id pos)) s =
    case getVar id s of
        Nothing -> return $ withVar id val s
        Just v  -> posFail pos $ "Redefinition of value `"++id++"`"
bindValue val (BFunctionDecl (Identifier _ pos) _) s = posFail pos "Function binding can only exist as a single let binding"

constructorStore as s = foldl (\s' (id,v) -> withVar id v s') s (map conv as)
    where
        conv (id :>: TFunction{}) = (id, FunctionValue $ Constr id)
        conv (id :>: _) = (id, UnionValue (id, UnionEnum))