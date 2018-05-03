module Translator where

import AST
import ProgramState

import TypeInferenceSystem

import Text.Parsec.Pos

import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad

import Data.List
import Data.Char

-- THIS SHOULD BE IN THE transformers LIBRARY
instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

translate :: AST.Program -> IO Store
translate (Program decls) = evalStateT inner initialInferenceState
    where
        inner :: TI Store
        inner = do
        let (types, annotations, assumptions, valDecls) = divideDecls decls
        lift $ print types
        lift $ print annotations
        lift $ print assumptions
        lift $ print valDecls
        checkTypes types annotations
        let valDs = divideDependant valDecls
        as <- typecheck valDs annotations assumptions
        lift $ print as
        return $ foldl valDeclToStore baseLibStore valDecls

typecheck :: [[ValueDeclaration]] -> [Assumption] -> [Assumption] -> TI [Assumption]
typecheck valDs annotations as = foldM (tiValDecls annotations) (as ++ baseLibTypes) valDs

checkTypes :: (Monad m) => [Either (Identifier, Type) Type] -> [Assumption] -> m ()
-- make sure that for each tvar in angles there is a TVar in body,
-- make sure no undefined name is used
-- make sure you cannot do a cyclic dependency in an alias or a record
checkTypes types annotations = return () -- TODO implement

divideDependant :: [ValueDeclaration] -> [[ValueDeclaration]]
-- group mutually recursive declarations 
-- sort based on dependency
divideDependant vds = [vds] -- TODO implement

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

        dataDecl (DataTypeDeclaration id tp uds) =
            let t = case tp of
                        EmptyTypeParam -> TUnion id uds
                        TypeParam tids -> 
                            let ids = map (\(TypeIdentifier id) -> id) tids in
                            TConstr ids $ TUnion id (map (applyUDvars ids) uds)
            in (Right t, constructors t)
            where
                applyUDvars vars u@(UDEnum _) = u
                applyUDvars vars u@(UDTyped id t) = UDTyped id (applyVars vars t)
                constructors :: Type -> [Assumption]
                constructors t@(TUnion id uds) = map (constr t) uds
                constructors (TConstr ids t@(TUnion id uds)) = map (constr t) uds
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

valDeclToStore :: Store -> ValueDeclaration -> Store
-- convert value declaration to a ProgramState.Value / Expression
-- previously sorted vds should remove laziness from values
valDeclToStore s vd = s -- TODO implement