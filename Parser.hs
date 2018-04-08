{-# LANGUAGE FlexibleContexts #-}
module Parser where

import AST

import Prelude hiding (maybe)
import qualified Data.Maybe

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language
import Data.Functor.Identity

lexer :: Stream s m Char => Tok.GenTokenParser s u m
lexer = Tok.makeTokenParser $ emptyDef{
            Tok.commentStart = "(*",
            Tok.commentEnd = "*)",
            Tok.commentLine = "//",
            Tok.nestedComments = True,
            Tok.identStart = letter <|> char '_',
            Tok.identLetter = alphaNum <|> char '_',
            Tok.opStart = oneOf ":!#$%&*+/<=>?\\^|-~",
            Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
            Tok.reservedNames = [
                "let", "with", "in", "match", "type",
                "data", "if", "then", "else", "do"
              ],
            Tok.reservedOpNames = [
                "(", ")", "_", "=", "@", "::", ",", "{",
                "}", ";", "|", "->", "[", "]", "()",
                ".", "..", "\\", "<-", "..."
              ]
        }

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = Tok.symbol lexer
parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = Tok.parens lexer
braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = Tok.braces lexer
angles :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
angles = Tok.angles lexer
brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = Tok.brackets lexer
semi :: Stream s m Char => ParsecT s u m String
semi = Tok.semi lexer
comma :: Stream s m Char => ParsecT s u m String
comma = Tok.comma lexer
colon :: Stream s m Char => ParsecT s u m String
colon = Tok.colon lexer
dot :: Stream s m Char => ParsecT s u m String
dot = Tok.dot lexer
integer :: Stream s m Char => ParsecT s u m Integer
integer = Tok.integer lexer
float :: Stream s m Char => ParsecT s u m Double
float = Tok.float lexer
stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = Tok.stringLiteral lexer
charLiteral :: Stream s m Char => ParsecT s u m Char
charLiteral = Tok.charLiteral lexer
identifier :: Stream s m Char => ParsecT s u m String
identifier = Tok.identifier lexer
reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = Tok.reserved lexer
operator :: Stream s m Char => ParsecT s u m String
operator = Tok.operator lexer
reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = Tok.reservedOp lexer
whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = Tok.whiteSpace lexer

---

parseFile filename contents = parse pProgram filename contents

pProgram :: Stream s m Char => ParsecT s u m Program
pProgram = whiteSpace *> many pDeclaration >>= return . Program

--- DECLARATIONS ---

pDeclaration :: Stream s m Char => ParsecT s u m Declaration
pDeclaration =
    pTypeDeclaration <|> pDataTypeDeclaration
    <|> pTypeAnnotation <|> pValueDeclaration

pTypeDeclaration :: Stream s m Char => ParsecT s u m Declaration
pTypeDeclaration = do
    reserved "type"
    typeId <- pIdentifier
    param <- pTypeParam
    td <- pTypeDefinition
    return (TypeDeclaration typeId param td)

pTypeParam :: Stream s m Char => ParsecT s u m TypeParam
pTypeParam = do
    mp <- maybe $ angles $ sepBy1 (pIdentifier >>= return . TypeIdentifier) comma
    case mp of
        Nothing -> return EmptyTypeParam
        Just ps -> return (TypeParam ps)

pTypeDefinition :: Stream s m Char => ParsecT s u m TypeDefinition
pTypeDefinition = try pTDAlias <|> pTDRec <|> pTDExt

pTDAlias :: Stream s m Char => ParsecT s u m TypeDefinition
pTDAlias = do
    reservedOp "="
    pType >>= return . TDAlias

pTDRec :: Stream s m Char => ParsecT s u m TypeDefinition
pTDRec = do
    reservedOp "="
    braces (sepBy1 pRecordFieldType semi) >>= return . TDRecord

pTDExt :: Stream s m Char => ParsecT s u m TypeDefinition
pTDExt = do
    reserved "extends"
    id <- pIdentifier
    reserved "with"
    recs <- braces (sepBy1 pRecordFieldType semi)
    return (TDExtension id recs)

pRecordFieldType :: Stream s m Char => ParsecT s u m RecordFieldType
pRecordFieldType = do
    id <- pIdentifier
    reservedOp "::"
    t <- pType
    return (RecordFieldType id t)

pDataTypeDeclaration :: Stream s m Char => ParsecT s u m Declaration
pDataTypeDeclaration = do
    reserved "data"
    id <- pIdentifier
    param <- pTypeParam
    reservedOp "="
    maybe (reservedOp "|")
    u <- sepBy1 pUnionDef (reservedOp "|")
    return (DataTypeDeclaration id param u)

pUnionDef :: Stream s m Char => ParsecT s u m UnionDefinition
pUnionDef = do
    id <- pIdentifier
    mu <- maybe (reserved "of" >> pType)
    case mu of
        Nothing -> return (UDEnum id)
        Just t -> return (UDTyped id t)

pTypeAnnotation :: Stream s m Char => ParsecT s u m Declaration
pTypeAnnotation = do
    reservedOp "@"
    id <- pIdentifier
    reservedOp "::"
    t <- pType
    return (TypeAnnotation id t)

pValueDeclaration :: Stream s m Char => ParsecT s u m Declaration
pValueDeclaration = do
    reserved "let"
    bind <- pBindPattern
    reservedOp "="
    expr <- pExpression
    return (ValueDeclaration bind expr)

--- TYPE ---

pType :: Stream s m Char => ParsecT s u m Type
pType = do
    t <- pConciseType
    mt2 <- maybe (pTupleType t <|> pFunctionType t)
    case mt2 of
        Nothing -> return t
        Just t2 -> return t2

pTupleType :: Stream s m Char => Type -> ParsecT s u m Type
pTupleType t1 = do
    whiteSpace *> symbol "*"
    t2 <- pType
    case t2 of
        TTuple tl -> return (TTuple (t1:tl))
        _ -> return (TTuple [t1, t2])

pFunctionType :: Stream s m Char => Type -> ParsecT s u m Type
pFunctionType t1 = do
    reservedOp "->"
    t2 <- pType
    return (TFunction t1 t2)

pConciseType :: Stream s m Char => ParsecT s u m Type
pConciseType = 
    pNamedType
        <|> try pUnitType
        <|> (parens pType >>= return . TParenthesis)
        <|> pListType
        <|> pRecordType

pUnitType :: Stream s m Char => ParsecT s u m Type
pUnitType = reservedOp "()" >> return TUnit

pNamedType :: Stream s m Char => ParsecT s u m Type
pNamedType = do
    id <- pIdentifier
    params <- pExactTypeParam
    return (TByName id params)

pExactTypeParam :: Stream s m Char => ParsecT s u m ExactTypeParam
pExactTypeParam = do
    m <- maybe $ angles $ sepBy1 pType comma
    case m of
        Nothing -> return ExEmptyTypeParam
        Just ts -> return (ExTypeParam ts)

pListType :: Stream s m Char => ParsecT s u m Type
pListType = brackets pType >>= return . TList

pRecordType :: Stream s m Char => ParsecT s u m Type
pRecordType = 
    braces (sepBy1 pRecordFieldType semi) >>= return . TRecord

--- EXPRESSION ---

pExpression :: Stream s m Char => ParsecT s u m Expression
pExpression = pOpExpr definedOperators

pOpExpr :: Stream s m Char => [[String]] -> ParsecT s u m Expression
pOpExpr (ops:t) = do
    let continuation = case t of
                        [] -> pDotExpr
                        _ -> pOpExpr t
    e1 <- continuation
    option e1 (try $ pInfix e1 continuation)
    where
        pInfix e1 continuation = do
            op <- whiteSpace *> pOp ops
            e2 <- continuation
            let opExpr = EOp e1 (Op op) e2
            case assoc op of
                LeftAssoc -> option opExpr (try $ pInfix opExpr continuation)
                RightAssoc -> do
                    me3 <- maybe (pInfix e2 continuation)
                    case me3 of
                        Nothing -> return opExpr
                        Just e3 -> return (EOp e1 (Op op) e3)
pOp :: Stream s m Char => [String] -> ParsecT s u m String
pOp [] = operator
pOp l = foldl (<|>) (unexpected "Op") $ map (try . symbol)  l

pDotExpr :: Stream s m Char => ParsecT s u m Expression
pDotExpr = do
    e <- pTypedExpr
    inner e
    where
        inner :: Stream s m Char => Expression -> ParsecT s u m Expression
        inner exp = do
            mi <- maybe (string "." *> pIdentifier)
            case mi of
                Nothing -> return exp
                Just id -> inner (ERecordField exp id)

pTypedExpr :: Stream s m Char => ParsecT s u m Expression
pTypedExpr = do
    e <- pApplicationExpr
    option e (try (reservedOp "::" *> pType >>= return . (ETyped e)))

pApplicationExpr :: Stream s m Char => ParsecT s u m Expression
pApplicationExpr = do
    e <- pConciseExpr
    whiteSpace
    inner e
    where
        inner :: Stream s m Char => Expression -> ParsecT s u m Expression
        inner exp = do
            me2 <- maybe ((parens $ sepBy1 pExpression comma) >>= return . (EApplication exp))
            case me2 of
                Nothing -> return exp
                Just e2 -> inner e2

pConciseExpr :: Stream s m Char => ParsecT s u m Expression
pConciseExpr = 
    pVarExpr <|> try pParensExpr <|> pLiteralExpr
    <|> pNegativeExpr <|> pLetExpr <|> pDoExpr
    <|> pTupleExpr <|> try pListSeqExpr <|> try pListComprehensionExpr
    <|> pListExpr <|> try pRecordExpr <|> pRecordUpdateExpr
    <|> try pIfExpr <|> pIfDoExpr <|> pLambdaExpr
    <|> pMatchExpr
    
pNegativeExpr :: Stream s m Char => ParsecT s u m Expression
pNegativeExpr = symbol "-" >> pExpression >>= return . ENegative

pParensExpr :: Stream s m Char => ParsecT s u m Expression
pParensExpr = parens pExpression >>= return . EParenthesis

pVarExpr :: Stream s m Char => ParsecT s u m Expression
pVarExpr = pIdentifier >>= return . EVariable

pLetExpr :: Stream s m Char => ParsecT s u m Expression
pLetExpr = do
    reserved "let"
    bp <- pBindPattern
    reservedOp "="
    eass <- pExpression
    maybe (reserved "in")
    eres <- pExpression
    return (ELet bp eass eres)

pLiteralExpr :: Stream s m Char => ParsecT s u m Expression
pLiteralExpr = pLiteral >>= return . ELiteral

pLiteral :: Stream s m Char => ParsecT s u m Literal
pLiteral =
    (stringLiteral >>= return . AST.String)
     <|> (charLiteral >>= return . AST.Char)
     <|> try (float >>= return . AST.Float)
     <|> try (integer >>= return . AST.Integer)
     <|> try (reservedOp "()" >> return UnitValue)

pDoExpr :: Stream s m Char => ParsecT s u m Expression
pDoExpr = 
    reserved "do" >> pExpression >>= return . EDo

pTupleExpr :: Stream s m Char => ParsecT s u m Expression
pTupleExpr = 
    parens (sepBy2 pExpression comma) >>= return . ETuple

pListSeqExpr :: Stream s m Char => ParsecT s u m Expression
pListSeqExpr = 
    brackets inner
    where
        inner :: Stream s m Char => ParsecT s u m Expression
        inner = do
            e1 <- pExpression
            reservedOp ".."
            e2 <- pExpression
            return (EListSequence e1 e2)

pListComprehensionExpr :: Stream s m Char => ParsecT s u m Expression
pListComprehensionExpr =
    brackets inner
    where
        inner :: Stream s m Char => ParsecT s u m Expression
        inner = do
            eres <- pExpression
            reservedOp "|"
            comps <- sepBy1 pComprehension comma
            return (EListComprehension eres comps)

pListExpr :: Stream s m Char => ParsecT s u m Expression
pListExpr = brackets (sepBy pExpression comma) >>= return . EList

pRecordExpr :: Stream s m Char => ParsecT s u m Expression
pRecordExpr =
    braces (sepBy1 pRecordAssignment semi)
     >>= return . ERecord

pRecordAssignment :: Stream s m Char => ParsecT s u m RecordFieldAssignment
pRecordAssignment = do
    id <- pIdentifier
    reservedOp "="
    e <- pExpression
    return (RecordFieldAssignment id e)

pRecordUpdateExpr :: Stream s m Char => ParsecT s u m Expression
pRecordUpdateExpr =
    braces inner
    where
        inner :: Stream s m Char => ParsecT s u m Expression
        inner = do
            id <- pIdentifier
            reserved "with"
            fields <- sepBy1 pRecordAssignment semi
            return (ERecordUpdate id fields)

pIfExpr :: Stream s m Char => ParsecT s u m Expression
pIfExpr = do
    reserved "if"
    econd <- pExpression
    reserved "then"
    etrue <- pExpression
    reserved "else"
    efalse <- pExpression
    return (EIf econd etrue efalse)

pIfDoExpr :: Stream s m Char => ParsecT s u m Expression
pIfDoExpr = do
    reserved "if"
    econd <- pExpression
    reserved "do"
    e <- pExpression
    return (EIfDo econd e)

pLambdaExpr :: Stream s m Char => ParsecT s u m Expression
pLambdaExpr = do
    reservedOp "\\"
    params <- many1 pParam
    reservedOp "->"
    e <- pExpression
    return (ELambda params e)

pMatchExpr :: Stream s m Char => ParsecT s u m Expression
pMatchExpr = do
    reserved "match"
    em <- pExpression
    reserved "with"
    maybe (reservedOp "|")
    alts <- sepBy1 pAlternate (reservedOp "|")
    return (EMatch em alts)

pAlternate :: Stream s m Char => ParsecT s u m Alternate
pAlternate = do
    pat <- pPattern
    reservedOp "->"
    e <- pExpression
    return (Alternate pat e)

pComprehension :: Stream s m Char => ParsecT s u m Comprehension
pComprehension = do
    bp <- pBindPattern
    reservedOp "<-"
    e <- pExpression
    return (Comprehension bp e)

--- PATTERN ---

pPattern :: Stream s m Char => ParsecT s u m Pattern
pPattern = 
    try pApplicationPat <|> pLiteralPat <|> try pWildCardPat
    <|> try pTuplePat <|> try pListHeadPat <|> pParensPat
    <|> pVarPat <|> try pListPat <|> pListContainsPat
    <|> pRecordPat

pVarPat :: Stream s m Char => ParsecT s u m Pattern
pVarPat = pIdentifier >>= return . PVariable

pApplicationPat :: Stream s m Char => ParsecT s u m Pattern
pApplicationPat = do
    id <- pIdentifier
    pat <- parens pPattern
    return (PApplication id pat)

pLiteralPat :: Stream s m Char => ParsecT s u m Pattern
pLiteralPat = pLiteral >>= return . PLiteral

pTuplePat :: Stream s m Char => ParsecT s u m Pattern
pTuplePat = parens (sepBy2 pPattern comma) >>= return . PTuple

pListPat :: Stream s m Char => ParsecT s u m Pattern
pListPat = brackets (sepBy pPattern comma) >>= return . PList

pListHeadPat :: Stream s m Char => ParsecT s u m Pattern
pListHeadPat = parens inner
    where
        inner :: Stream s m Char => ParsecT s u m Pattern
        inner = do
            phead <- pPattern
            whiteSpace *> symbol ":"
            ptail <- pPattern
            return (PListHead phead ptail)

pParensPat :: Stream s m Char => ParsecT s u m Pattern
pParensPat = parens pPattern >>= return . PParenthesis

pWildCardPat :: Stream s m Char => ParsecT s u m Pattern
pWildCardPat = symbol "_" >> return PWildCard

pListContainsPat :: Stream s m Char => ParsecT s u m Pattern
pListContainsPat = brackets inner
    where
        inner :: Stream s m Char => ParsecT s u m Pattern
        inner = do
            reservedOp "..."
            e <- pExpression
            reservedOp "..."
            return (PListContains e)

pRecordPat :: Stream s m Char => ParsecT s u m Pattern
pRecordPat = 
    braces (sepBy1 pRecordAssignmentPat semi) >>= return . PRecord

pRecordAssignmentPat :: Stream s m Char => ParsecT s u m RecordPattern
pRecordAssignmentPat = do
    id <- pIdentifier
    reservedOp "="
    p <- pPattern
    return (RecordPattern id p)

--- BIND PATTERN ---

pBindPattern :: Stream s m Char => ParsecT s u m BindPattern
pBindPattern =
    try pBindParenthesis
        <|> pBindOp
        <|> pBindRecord
        <|> pBindList
        <|> (reservedOp "_" >> return BWildCard)
        <|> (do
            id <- pIdentifier
            r <- maybe $ pBindFunctionDecl id
            case r of
                Nothing -> return (BVariable id)
                Just r_ -> return r_
            )

pBindFunctionDecl :: Stream s m Char => Identifier -> ParsecT s u m BindPattern
pBindFunctionDecl id = do
    r <- many1 pParam
    return (BFunctionDecl id r)

pParam :: Stream s m Char => ParsecT s u m Param
pParam =
    (reservedOp "()" >> return Unit)
        <|> (reservedOp "_"  >> return WildCard)
        <|> (pIdentifier >>= return . Parameter)

pBindParenthesis :: Stream s m Char => ParsecT s u m BindPattern
pBindParenthesis =
    parens $ try pBindListHead
            <|> try pBindTuple
            <|> (pBindPattern >>= return . BParenthesis)
    where
        pBindListHead :: Stream s m Char => ParsecT s u m BindPattern
        pBindListHead = do
            b1 <- pBindPattern
            symbol ":"
            b2 <- pBindPattern
            return (BListHead b1 b2)

        pBindTuple :: Stream s m Char => ParsecT s u m BindPattern
        pBindTuple = sepBy2 pBindPattern comma >>= return . BTuple

pBindOp :: Stream s m Char => ParsecT s u m BindPattern
pBindOp = do
    op <- parens operator >>= return . Op
    r <- many1 pParam
    return (BOp op r)

pBindRecord :: Stream s m Char => ParsecT s u m BindPattern
pBindRecord =
    (braces $ sepBy1 pBindRecordElem comma) >>= return . BRecord
    where
        pBindRecordElem :: Stream s m Char => ParsecT s u m RecordBindPattern
        pBindRecordElem = do
            id <- pIdentifier
            symbol "->"
            varId <- pBindPattern
            return (RecordBindPattern id varId)

pBindList :: Stream s m Char => ParsecT s u m BindPattern
pBindList =
    (brackets $ sepBy pBindPattern comma) >>= return . BList


pIdentifier :: Stream s m Char => ParsecT s u m Identifier
pIdentifier = identifier >>= return . Identifier

sepBy2 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
sepBy2 p sep = do
    w <- p
    _ <- sep
    ws <- sepBy1 p sep
    return (w:ws)


maybe :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe a)
maybe p = option Nothing (try p >>= return . Just)

definedOperators = [
    [],
    ["<|","<||","<|||","|>","||>","|||>",
      "<$>", "<$", "$>"],
    ["<", "<=", "==", "===", ">", ">=",
      "!=", "/=", "=/=", "!==", "/=="],
    ["||", "<<", ">>", "<=>", "<==", "==>", ">=>", "<=<", "~>", "<~", "<<=", "=>>", "=<<", ">>="],
    ["+", "-","&&"],
    ["*", "/"],
    ["**", "***", "%", "^", "&&&", "&", "|||", "++", "--"]
  ]

data OpAssoc = LeftAssoc | RightAssoc

assoc op =
    let d = ["<|","<||","<|||","<$","<$>","<<=","<==",
            "=<<","<=<","<~","<<","**","***"] in
        if elem op d then RightAssoc else LeftAssoc
