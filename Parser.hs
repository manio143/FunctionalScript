{-# LANGUAGE FlexibleContexts #-}
module Parser where

import AST

import Prelude hiding (maybe)

import Control.Monad
import Text.Parsec hiding (token, spaces)
import Text.Parsec.String
import Data.Functor.Identity

parseFile :: ParsecT String () Identity Program -> String -> String -> Either ParseError Program
parseFile p filename contents = parse p filename contents

pProgram :: Stream s m Char => ParsecT s u m Program
pProgram = endBy pDeclaration newlines1 >>= return . Program

pDeclaration :: Stream s m Char => ParsecT s u m Declaration
pDeclaration = 
    pTypeDeclaration
      <|> pDataTypeDeclaration
      <|> pTypeAnnotation
      <|> pValueDeclaration

pTypeDeclaration :: Stream s m Char => ParsecT s u m Declaration
pTypeDeclaration = do
    token "type"
    typeId <- pIdentifier
    typeGenericIdList <- option [] pTypeIdentifierList
    typeDef <- pTypeDefinition
    return (TypeDeclaration typeId typeGenericIdList typeDef)

pDataTypeDeclaration :: Stream s m Char => ParsecT s u m Declaration
pDataTypeDeclaration = do
    token "data"
    typeId <- pIdentifier
    typeGenericIdList <- option [] pTypeIdentifierList
    rbtoken "="
    unionDefinition <- pUnionDefinitionList
    return (DataTypeDeclaration typeId typeGenericIdList unionDefinition)

pTypeAnnotation :: Stream s m Char => ParsecT s u m Declaration
pTypeAnnotation = do
    rtoken "@"
    valueId <- pIdentifier 
    token "::"
    type_ <- pType
    return (TypeAnnotation valueId type_)

pValueDeclaration :: Stream s m Char => ParsecT s u m Declaration
pValueDeclaration = do
    token "let"
    bind <- pBindPattern <?> "identifier"
    rbtoken "="
    expr <- pExpression
    return (ValueDeclaration bind expr)

pTypeDefinition :: Stream s m Char => ParsecT s u m TypeDefinition
pTypeDefinition = pTypeAssignment <|> pTypeExtension

pTypeAssignment :: Stream s m Char => ParsecT s u m TypeDefinition
pTypeAssignment = do
    rbtoken "="
    pRecordTypeDefinition 
      <|> pAliasDefinition 

pTypeExtension :: Stream s m Char => ParsecT s u m TypeDefinition
pTypeExtension = do
    token "extends" 
    typeId <- pIdentifier
    token "with"
    rd <- pRecordTypeDefinition
    case rd of
        TDRecord rd_ -> return (TDExtension typeId rd_)

pRecordTypeDefinition :: Stream s m Char => ParsecT s u m TypeDefinition
pRecordTypeDefinition = do
    rbtoken "{"
    rd <- pRecordFieldTypeDefinitionList
    lbtoken "}"
    return (TDRecord rd)

pRecordFieldTypeDefinitionList :: Stream s m Char => ParsecT s u m [RecordTypeDefinition]
pRecordFieldTypeDefinitionList = sepBy1 (
        do
            fieldId <- pIdentifier
            token "::"
            t <- pType
            return (RecordTypeDefinition fieldId t)
    ) comma

pAliasDefinition :: Stream s m Char => ParsecT s u m TypeDefinition
pAliasDefinition = pType >>= return . TDAlias

pUnionDefinitionList :: Stream s m Char => ParsecT s u m [UnionDefinition]
pUnionDefinitionList = do
    _ <- maybe $ token "|"
    pUnionElementDefinition `sepBy1` (lbtoken "|")

pUnionElementDefinition :: Stream s m Char => ParsecT s u m UnionDefinition
pUnionElementDefinition =
    try pUnionElementWithDataDefinition 
      <|> (pIdentifier >>= return . UnionDefinitionEnum)

pUnionElementWithDataDefinition :: Stream s m Char => ParsecT s u m UnionDefinition 
pUnionElementWithDataDefinition = do
    id <- pIdentifier
    token "of"
    t <- pType
    return (UnionDefinitionTyped id t)

pType :: Stream s m Char => ParsecT s u m Type
pType = do
    t1 <- pConciseType
    mt2 <- maybe (try (pTupleType t1) <|> try (pFunctionType t1))
    case mt2 of
        Nothing -> return t1
        Just t2 -> return t2

pConciseType :: Stream s m Char => ParsecT s u m Type
pConciseType = 
    try pUnitType
      <|> try pNamedType
      <|> try (parenthesis pType >>= return . TParenthesis)
      <|> try pListType
      <|> try pRecordType

pUnitType :: Stream s m Char => ParsecT s u m Type
pUnitType = token "()" >> return (TUnit)

pListType :: Stream s m Char => ParsecT s u m Type
pListType = do
    token "["
    t <- pType
    token "]"
    return (TList t)
    
pTupleType :: Stream s m Char => Type -> ParsecT s u m Type
pTupleType t1 = do
    token "*"
    t2 <- pType
    case t2 of
        TTuple tl -> return (TTuple (t1:tl))
        _ -> return (TTuple [t1, t2])

pFunctionType :: Stream s m Char => Type -> ParsecT s u m Type
pFunctionType t1 = do
    token "->"
    t2 <- pType
    return (TFunction t1 t2)

pNamedType :: Stream s m Char => ParsecT s u m Type
pNamedType = do
    id <- pIdentifier
    types <- option [] pGenericTypeArgumentsList
    return (TByName id types)

pRecordType :: Stream s m Char => ParsecT s u m Type
pRecordType = do
    rd <- pRecordTypeDefinition
    case rd of
        TDRecord rd_ -> return (TRecord rd_)


pExpression :: Stream s m Char => ParsecT s u m Expression
pExpression = return (EVariable $ Identifier "x")
pBindPattern :: Stream s m Char => ParsecT s u m BindPattern
pBindPattern =
    pBindParenthesis
      <|> pBindRecord
      <|> pBindList
      <|> (try (rtoken "_" <* oneOf " \t") >> return BWildCard)
      <|> (do
            id <- pIdentifier
            r <- maybe $ pBindFunctionDecl id
            case r of
                Nothing -> return (BVariable id)
                Just r_ -> return r_
            )

pBindFunctionDecl :: Stream s m Char => Identifier -> ParsecT s u m BindPattern
pBindFunctionDecl id = do
    whiteSpace
    r <- endBy1 pParam whiteSpace
    return (BFunctionDecl id r)

pParam :: Stream s m Char => ParsecT s u m Param
pParam =
    (string "()" >> return Unit)
      <|> try (string "_" <* notFollowedBy pIdentifier  >> return WildCard)
      <|> (pIdentifier >>= return . Parameter)

pBindParenthesis :: Stream s m Char => ParsecT s u m BindPattern
pBindParenthesis = do
    token "("
    b <- try pBindListHead
            <|> try pBindTuple
            <|> (pBindPattern >>= return . BParenthesis)
    token ")"
    return b
    where
        pBindListHead :: Stream s m Char => ParsecT s u m BindPattern
        pBindListHead = do
            b1 <- pBindPattern
            token ":"
            b2 <- pBindPattern
            return (BListHead b1 b2)

        pBindTuple :: Stream s m Char => ParsecT s u m BindPattern
        pBindTuple = sepBy2 pBindPattern comma >>= return . BTuple

pBindRecord :: Stream s m Char => ParsecT s u m BindPattern
pBindRecord = do
    token "{"
    rd <- sepBy1 pBindRecordElem comma
    token "}"
    return (BRecord rd)
    where
        pBindRecordElem :: Stream s m Char => ParsecT s u m RecordBindPattern
        pBindRecordElem = do
            id <- pIdentifier
            token "->"
            varId <- pIdentifier
            return (RecordBindPatternElem id varId)

pBindList :: Stream s m Char => ParsecT s u m BindPattern
pBindList = do
    token "["
    rd <- sepBy pIdentifier comma
    token "]"
    return (BList rd)


pGenericTypeArgumentsList :: Stream s m Char => ParsecT s u m [Type]
pGenericTypeArgumentsList = do
    ltoken "<"
    identifiers <- sepBy1 pType comma
    rtoken ">"
    return identifiers

pTypeIdentifierList :: Stream s m Char => ParsecT s u m [TypeIdentifier]
pTypeIdentifierList = do
    ltoken "<"
    identifiers <- sepBy1 pTypeIdentifier comma
    rtoken ">"
    return identifiers

pIdentifier :: Stream s m Char => ParsecT s u m Identifier
pIdentifier = 
    startWithOneContinueWithMany (letter <|> char '_') (letter <|> digit <|> char '_')
      >>= return . Identifier

pTypeIdentifier :: Stream s m Char => ParsecT s u m TypeIdentifier
pTypeIdentifier =
    startWithOneContinueWithMany letter (letter <|> digit)
      >>= return . TypeIdentifier

pComment :: Stream s m Char => ParsecT s u m String
pComment = string "//" <* manyTill anyToken (string "\n")
       <|> string "(*" <* manyTill anyToken (string "*)")

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = void $ (many $ ((try pComment) <|> string " " <|> string "\t" <?> "whitespace"))

-- spaces :: Stream s m Char => ParsecT s u m ()
-- spaces = void $ many $ ((string " " <|> string "\t") <?> "whitespace")

newlines :: Stream s m Char => ParsecT s u m ()
newlines = _newlines many

newlines1 :: Stream s m Char => ParsecT s u m ()
newlines1 = _newlines many1

_newlines :: Stream s m Char => (ParsecT s u m String -> ParsecT s u m [String]) -> ParsecT s u m ()    
_newlines combinator = void $ whiteSpace *> ((combinator $ (string "\n")) <?> "a new line")
                 

comma :: Stream s m Char => ParsecT s u m ()
comma = rbtoken ","

ltoken :: Stream s m Char => String -> ParsecT s u m ()
ltoken s = void $ string s <* whiteSpace

rtoken :: Stream s m Char => String -> ParsecT s u m ()
rtoken s = void $ whiteSpace *> string s

token :: Stream s m Char => String -> ParsecT s u m ()
token s = void $ whiteSpace *> string s <* whiteSpace

rbtoken :: Stream s m Char => String -> ParsecT s u m ()
rbtoken s = void $ whiteSpace *> string s <* newlines <* whiteSpace

lbtoken :: Stream s m Char => String -> ParsecT s u m ()
lbtoken s = void $ (try $ newlines *> whiteSpace *> string s) <* whiteSpace

asArray :: Stream s m Char => a -> ParsecT s u m [a]
asArray = return . (:[])

startWithOneContinueWithMany :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m [a]
startWithOneContinueWithMany one_ many_ = (++) <$> (one_ >>= asArray) <*> many many_

parenthesis :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parenthesis p = do
    token "("
    res <- p
    token ")"
    return res

sepBy2 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
sepBy2 p sep = do
    w <- p
    _ <- sep
    ws <- sepBy1 p sep
    return (w:ws)


maybe :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe a)
maybe p = option Nothing (try p >>= return . Just)
