module AST where

  import Text.Parsec

  data Identifier = Identifier String SourcePos
    deriving (Eq, Ord, Show)
  data Op = Op String
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
    deriving (Eq, Ord, Show)
  
  data UnionDefinition = UDEnum Identifier | UDTyped Identifier Type
    deriving (Eq, Ord, Show)
  
  data Type
      = TFunction SourcePos Type Type
      | TTuple [Type]
      | TList SourcePos Type
      | TUnit SourcePos
      | TByName Identifier ExactTypeParam
      | TRecord [RecordFieldType]
      | TParenthesis Type
    deriving (Eq, Ord, Show)
  
  data ExactTypeParam = ExTypeParam [Type] | ExEmptyTypeParam
    deriving (Eq, Ord, Show)
  
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