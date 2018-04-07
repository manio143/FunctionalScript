{-# LANGUAGE FlexibleContexts #-}
module Parser where

import AST

import Prelude hiding (maybe)
import qualified Data.Maybe

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
    lbtoken "type"
    typeId <- pIdentifier
    typeGenericIdList <- option [] pTypeIdentifierList
    typeDef <- pTypeDefinition
    return (TypeDeclaration typeId typeGenericIdList typeDef)

pDataTypeDeclaration :: Stream s m Char => ParsecT s u m Declaration
pDataTypeDeclaration = do
    lbtoken "data"
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
    lbtoken "let"
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




-- pExpression :: Stream s m Char => ParsecT s u m Expression
-- pExpression = do
--     e1 <- pConciseExpression
--     me2 <- maybe (pDotExpr e1 <|> pWithTypeExpr e1 <|> pInfixOpExpr e1 <|> pApplicationExpr e1)
--     case me2 of
--         Nothing -> return e1
--         Just e2 -> return e2

pConciseExpression :: Stream s m Char => ParsecT s u m Expression
pConciseExpression = 
    pNegativeExpr <|> pLetExpr {-<|> TODO pMatchExpr-}
     <|> pLiteral <|> pVariableExpr <|>
     pDoExpr <|> try pTupleExpr <|> pListExpr <|>
     pRecordExpr <|> pIfThenElseExpr <|> (parenthesis pExpression >>= return . EParenthesis) <|> pLambdaExpr

pNegativeExpr :: Stream s m Char => ParsecT s u m Expression
pNegativeExpr = do
    token "-"
    e <- pExpression
    return (ENegative e)

pLetExpr :: Stream s m Char => ParsecT s u m Expression
pLetExpr = do
    lbtoken "let"
    bp <- pBindPattern
    rbtoken "="
    e1 <- pExpression
    rbtoken "in" <|> newlines1
    e2 <- pExpression
    return (ELet bp e1 e2)

pLiteral :: Stream s m Char => ParsecT s u m Expression
pLiteral = (pStringLiteral <|> pNumLiteral <|> pCharLiteral) >>= return . ELiteral

pStringLiteral :: Stream s m Char => ParsecT s u m Literal
pStringLiteral = do
    rtoken "\""
    str <- manyTill pReadChar (ltoken "\"")
    return (LString str)

pReadChar :: Stream s m Char => ParsecT s u m Char
pReadChar = pReadEscapedChar <|> anyChar
pReadEscapedChar :: Stream s m Char => ParsecT s u m Char
pReadEscapedChar = do
    string "\\"
    c <- anyChar
    case c of
        'n' -> return '\n'
        't' -> return '\t'
        '\\' -> return '\\'
        _ -> unexpected "invalid escape sequence"

pNumLiteral :: Stream s m Char => ParsecT s u m Literal
pNumLiteral = do
    num1 <- many1 digit
    mn2 <- maybe (string "." *> many1 digit)
    case mn2 of
        Nothing -> return . LInteger $ (read num1 :: Integer)
        Just num2 -> return . LFloat $ (read (num1 ++ "." ++ num2) :: Double)

pCharLiteral :: Stream s m Char => ParsecT s u m Literal
pCharLiteral = do
    rtoken "'"
    c <- pReadChar
    ltoken "'"
    return (LChar c)

pVariableExpr :: Stream s m Char => ParsecT s u m Expression
pVariableExpr = pIdentifier >>= return . EVariable

pDoExpr :: Stream s m Char => ParsecT s u m Expression
pDoExpr = do
    lbtoken "do"
    e <- pExpression
    return (EDo e)

pIfThenElseExpr :: Stream s m Char => ParsecT s u m Expression
pIfThenElseExpr = do
    lbtoken "if"
    cond <- pExpression
    lbtoken "then"
    tr <- pExpression
    lbtoken "else"
    fl <- pExpression
    return (EIf cond tr fl)

pLambdaExpr :: Stream s m Char => ParsecT s u m Expression
pLambdaExpr = do
    lbtoken "\\"
    p <- endBy1 pParam whiteSpace
    token "->"
    body <- pExpression
    return (ELambda p body)

pTupleExpr :: Stream s m Char => ParsecT s u m Expression
pTupleExpr = do
    token "("
    exps <- sepBy2 pExpression comma
    token ")"
    return (ETuple exps)

pListExpr :: Stream s m Char => ParsecT s u m Expression
pListExpr = do
    token "["
    e <- try pListCommaSeparated <|> try pRange <|> pComprehension
    token "]"
    return e
    where
        pListCommaSeparated :: Stream s m Char => ParsecT s u m Expression
        pListCommaSeparated = sepBy pExpression comma >>= return . EList
        pRange :: Stream s m Char => ParsecT s u m Expression
        pRange = do
            e1 <- pExpression
            token ".."
            e2 <- pExpression
            return (EListRange e1 e2)
        pComprehension :: Stream s m Char => ParsecT s u m Expression
        pComprehension = do
            e1 <- pExpression
            token "|"
            stms <- sepBy1 pStatement comma
            return (EListComprehension e1 stms)
        pStatement :: Stream s m Char => ParsecT s u m Statement
        pStatement = do
            bp <- pBindPattern
            token "<-"
            e <- pExpression
            return (Statement bp e)

pRecordExpr :: Stream s m Char => ParsecT s u m Expression
pRecordExpr = do
    token "{"
    e <- try pWithRec <|> pNewRec
    token "}"
    return e
    where
        pWithRec :: Stream s m Char => ParsecT s u m Expression
        pWithRec = do
            var <- pIdentifier
            rbtoken "with"
            ERecordConstruction recs <- pNewRec
            return (ERecordUpdate var recs)
        pNewRec :: Stream s m Char => ParsecT s u m Expression
        pNewRec = sepBy1 pFieldAssignment comma >>= return . ERecordConstruction
        pFieldAssignment :: Stream s m Char => ParsecT s u m RecordValue
        pFieldAssignment = do
            id <- pIdentifier
            token "="
            e <- pExpression
            return (RecordValue id e)

pExpression :: Stream s m Char => ParsecT s u m Expression
pExpression = pOpExpression [PipeLevel, ComparisonLevel, SubArithmetic, Arithmetic1, Arithmetic2, Arithmetic3]

pOpExpression :: Stream s m Char => [OpLevel] -> ParsecT s u m Expression
pOpExpression (x:y:xs) = do
    e1 <- pOpExpression (y:xs)
    me2 <- maybe (pInfix e1)
    case me2 of
        Nothing -> return e1
        Just e2 -> return e2
    where
        pInfix e1 = do
            ops <- newlines *> pOp <* newlines
            e2 <- pOpExpression (y:xs)
            let ass = assoc ops
            let op = Operator ops x
            case ass of
                LeftAssoc -> do
                    me3 <- maybe (pInfix (EInfixOp e1 op e2))
                    case me3 of
                        Nothing -> return (EInfixOp e1 op e2)
                        Just e3 -> return e3
                RightAssoc -> do
                    me3 <- maybe (pInfix e2)
                    case me3 of
                        Nothing -> return (EInfixOp e1 op e2)
                        Just e3 -> return (EInfixOp e1 op e3)
        pOp :: Stream s m Char => ParsecT s u m String
        pOp =
            lookup2 x definedOperators
            $> Data.Maybe.maybe unexpectedOp 
                (foldl (<|>) unexpectedOp . (map (try . string)))


pOpExpression [x] = do
    e1 <- pDotExpr
    me2 <- maybe (pInfix e1)
    case me2 of
        Nothing -> return e1
        Just e2 -> return e2
    where
        pInfix e1 = do
            ops <- newlines *> pOp <* newlines
            e2 <- pDotExpr
            let ass = assoc ops
            let op = Operator ops x
            case ass of
                LeftAssoc -> do
                    me3 <- maybe (pInfix (EInfixOp e1 op e2))
                    case me3 of
                        Nothing -> return (EInfixOp e1 op e2)
                        Just e3 -> return e3
                RightAssoc -> do
                    me3 <- maybe (pInfix e2)
                    case me3 of
                        Nothing -> return (EInfixOp e1 op e2)
                        Just e3 -> return (EInfixOp e1 op e3)
        pOp :: Stream s m Char => ParsecT s u m String
        pOp =
            lookup2 x definedOperators
            $> Data.Maybe.maybe unexpectedOp
                (foldl (<|>) unexpectedOp . (map (try . string)))

unexpectedOp :: Stream s m Char => ParsecT s u m String
unexpectedOp = unexpected "undefined operator"

pDotExpr :: Stream s m Char => ParsecT s u m Expression
pDotExpr = do
    e <- pWithTypeExpr
    mi <- maybe (string "."  *> pIdentifier)
    case mi of
        Nothing -> return e
        Just id -> return (ERecordField e id)

pWithTypeExpr :: Stream s m Char => ParsecT s u m Expression
pWithTypeExpr = do
    e <- pApplicationExpr
    mt <- maybe (token "::" *> pType)
    case mt of
        Nothing -> return e
        Just t -> return (EExpressionWithTypeSig e t)

pApplicationExpr :: Stream s m Char => ParsecT s u m Expression
pApplicationExpr = do
    e1 <- pConciseExpression
    me2 <- maybe (whiteSpace *> pExpression)
    case me2 of
        Nothing -> return e1
        Just e2 -> return (EApplication e1 e2)

pBindPattern :: Stream s m Char => ParsecT s u m BindPattern
pBindPattern =
    try pBindParenthesis
      <|> pBindOp
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

pBindOp :: Stream s m Char => ParsecT s u m BindPattern
pBindOp = do
    token "("
    op <- pOperator
    token ")"
    r <- endBy1 pParam whiteSpace
    return (BOpDecl op r)

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


pOperator :: Stream s m Char => ParsecT s u m Op
pOperator = do
    opLiteral <- many1 $ oneOf "<>?/|:+=-*&^%$#!~"
    let level = findIn definedOperators opLiteral
    case level of
        Nothing -> return (Operator opLiteral SubArithmetic)
        Just l -> return (Operator opLiteral l)

    where
        findIn :: [([String], OpLevel)] -> String -> Maybe OpLevel
        findIn [] lit = Nothing
        findIn (x:xs) lit | elem lit (fst x) = Just $ snd x
                          | otherwise = findIn xs lit

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

definedOperators = [
    (["<|","<||","<|||","|>","||>","|||>",
      "<$>", "<$", "$>"], PipeLevel),
    (["<", "<=", "==", "===", ">", ">=",
      "!=", "/=", "=/=", "!==", "/=="], ComparisonLevel),
    (["||", "<<", ">>", "<=>", "<==", "==>", ">=>", "<=<", "~>", "<~", "<<=", "=>>", "=<<", ">>="], SubArithmetic),
    (["+", "-","&&"], Arithmetic1),
    (["*", "/"], Arithmetic2),
    (["**", "***", "%", "^", "&", "|", "++", "--"], Arithmetic3)
  ]

assoc op =
    let d = ["<|","<||","<|||","<$","<$>","<<=","<==",
             "=<<","<=<","<~","<<","**","***"] in
        if elem op d then RightAssoc else LeftAssoc

lookup2 x [] = Nothing
lookup2 x ((a,b):xs) | x == b = Just a
                     | otherwise = lookup2 x xs


($>) a b = b $ a