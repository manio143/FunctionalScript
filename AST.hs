module AST where

  import Data.List
  import Text.Parsec

  type Ident = String

  data Identifier = Identifier Ident SourcePos
    deriving (Eq, Ord, Show)
  data Op = Op Ident
    deriving (Eq, Ord, Show)
  
  data Param = Parameter Identifier | Unit SourcePos | WildCard SourcePos
    deriving (Eq, Ord, Show)
  
  data Program = Program [Declaration]
    deriving (Eq, Ord, Show)
  
  data Declaration
      = TypeDeclaration Identifier TypeParam TypeDefinition
      | DataTypeDeclaration Identifier TypeParam [UnionDefinition]
      | TypeAnnotation Identifier Type
      | ValueDeclaration BindPattern Expression
    deriving (Eq, Ord, Show)
  
  data ValueDeclaration = Val SourcePos BindPattern Expression
    deriving(Eq, Ord, Show)

  data TypeParam = TypeParam [TypeIdentifier] | EmptyTypeParam
    deriving (Eq, Ord, Show)
  
  data TypeIdentifier = TypeIdentifier Identifier
    deriving (Eq, Ord, Show)
  
  data TypeDefinition
      = TDAlias Type
      | TDRecord [RecordFieldType]
      | TDExtension Identifier [RecordFieldType]
    deriving (Eq, Ord, Show)
  
  data RecordFieldType = RecordFieldType Identifier Type
    deriving (Eq, Ord)

  instance Show RecordFieldType where
    show (RecordFieldType (Identifier id _) t) = id ++ " :: " ++ show t
  
  data UnionDefinition = UDEnum Identifier | UDTyped Identifier Type
    deriving (Eq, Ord, Show)
  
  data Type
      = TFunction SourcePos Type Type
      | TTuple [Type]
      | TList SourcePos Type
      | TUnit SourcePos
      | TNamed Identifier
      | TAlias Identifier Type
      | TRecord [RecordFieldType]
      | TParenthesis Type
      | TUnion Identifier [UnionDefinition]
      | TVar Identifier
      | TConstr [Identifier] Type
      | TApp SourcePos Type [Type]
    deriving (Eq, Ord)

  instance Show Type where
    show (TFunction _ t1 t2) = 
      (case t1 of
        TFunction{} -> show (TParenthesis t1)
        _ -> show t1) ++ " -> " ++ show t2
    show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
    show (TList _ t) = "[" ++ show t ++ "]"
    show (TUnit _) = "()"
    show (TNamed (Identifier id _)) = id
    show (TAlias (Identifier id _) _) = id
    show (TRecord rfts) = "{" ++ intercalate "; " (map show rfts) ++ "}"
    show (TParenthesis t) = "("++ show t ++ ")"
    show (TUnion (Identifier id _) _) = id
    show (TVar (Identifier id _)) = id
    show (TConstr ids t) = "|<"++intercalate "," (map (\(Identifier id _) -> id) ids) ++ "> => " ++ show t ++"|"
    show (TApp _ t ts) = show t ++ "<" ++ intercalate ", " (map show ts) ++ ">"
    
  data Literal
      = Char Char
      | String String
      | Integer Integer
      | Float Double
      | UnitValue
    deriving (Eq, Ord, Show)
  
  data Expression
      = EOp SourcePos Expression Op Expression --operand pos
      | ERecordField SourcePos Expression Identifier --dot pos
      | ETyped SourcePos Expression Type -- :: pos
      | EApplication SourcePos Expression [Expression] --par pos
      | ENegative SourcePos Expression
      | ELet SourcePos BindPattern Expression Expression
      | ELiteral SourcePos Literal
      | EVariable Identifier
      | EDo SourcePos Expression Expression
      | EParenthesis Expression
      | ETuple SourcePos [Expression]
      | EListSequence SourcePos Expression Expression
      | EListComprehension SourcePos Expression [Comprehension]
      | EList SourcePos [Expression]
      | ERecord SourcePos [RecordFieldAssignment]
      | ERecordUpdate SourcePos Identifier [RecordFieldAssignment]
      | EIf SourcePos Expression Expression Expression
      | EIfDo SourcePos Expression Expression Expression
      | ELambda SourcePos [Param] Expression
      | EMatch SourcePos Expression [Alternate]
    deriving (Eq, Ord, Show)
  
  data RecordFieldAssignment
      = RecordFieldAssignment SourcePos Identifier Expression
    deriving (Eq, Ord, Show)
  
  data Comprehension = Comprehension SourcePos BindPattern Expression
    deriving (Eq, Ord, Show)
  
  data Alternate = Alternate SourcePos Pattern Expression
    deriving (Eq, Ord, Show)
  
  data Pattern
      = PVariable Identifier
      | PLiteral SourcePos Literal
      | PApplication SourcePos Identifier Pattern
      | PTuple SourcePos [Pattern]
      | PList SourcePos [Pattern]
      | PListHead SourcePos Pattern Pattern
      | PListContains SourcePos Expression
      | PParenthesis Pattern
      | PWildCard SourcePos
      | PRecord SourcePos [RecordPattern]
    deriving (Eq, Ord, Show)
  
  data RecordPattern = RecordPattern SourcePos Identifier Pattern
    deriving (Eq, Ord, Show)
  
  data BindPattern
      = BParenthesis BindPattern
      | BOp SourcePos Op [Param]
      | BRecord SourcePos [RecordBindPattern]
      | BList SourcePos [BindPattern]
      | BTuple SourcePos [BindPattern]
      | BListHead SourcePos BindPattern BindPattern
      | BWildCard SourcePos
      | BVariable Identifier
      | BFunctionDecl Identifier [Param]
    deriving (Eq, Ord, Show)
  
  data RecordBindPattern = RecordBindPattern SourcePos Identifier BindPattern
    deriving (Eq, Ord, Show)  