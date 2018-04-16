module Translator where

import AST
import ProgramState

translate :: AST.Program -> Store
translate (Program decls) =
    let (typeDecls, annotations, valueDecls) = declSort decls in
    let types = transTypes typeDecls in
    let typeAnnon = transAnnon annotations in
    transValues valueDecls typeAnnon types


declSort :: [AST.Declaration] -> ([AST.Declaration], [AST.Declaration], [AST.Declaration])
declSort d = filter_pom d [] [] []
    where
        filter_pom [] t a v = (reverse t, reverse a, reverse v)
        filter_pom (h:tl) t a v = 
            case h of
                TypeDeclaration{} -> filter_pom tl (h:t) a v
                DataTypeDeclaration{} -> filter_pom tl (h:t) a v
                TypeAnnotation{} -> filter_pom tl t (h:a) v
                ValueDeclaration{} -> filter_pom tl t a (h:v)

transTypes :: [Declaration] -> [(Ident, ProgramState.Type)]
transTypes tds =
    convert tds []
    where
        convert (h:t) acc = convert t (convertSingle h : acc)
        convert [] acc = reverse acc
        convertSingle (TypeDeclaration id param def) =
            GenericType $ \extparam ->
                if length extparam != length param then
                    errror ("Incorrect number of type parametrs")
                else
                case def of
                    TDAlias t -> 
                        let tc = convertType t (param, extparam) in
                        AliasType tc
                    TDRecord rfts ->
                        RecordType $ map (\(RecordFieldType i tt) -> (i, convertType tt (param, extparam))) rfts
                    TDExtension fid rfts ->

--todo obsadź wszystko w monadę stanu, której stanem będzie słownik nowych typów