module AST where

  newtype Identifier = Identifier String
    deriving (Eq, Ord, Show, Read)
  data Op = Op String
    deriving (Eq, Ord, Show, Read)
  
  data Param = Parameter Identifier | Unit | WildCard
    deriving (Eq, Ord, Show, Read)
  
  data Program = Program [Declaration]
    deriving (Eq, Ord, Show, Read)
  
  data Declaration
      = TypeDeclaration Identifier TypeParam TypeDefinition
      | DataTypeDeclaration Identifier TypeParam [UnionDefinition]
      | TypeAnnotation Identifier Type
      | ValueDeclaration BindPattern Expression
    deriving (Eq, Ord, Show, Read)
  
  data TypeParam = TypeParam [TypeIdentifier] | EmptyTypeParam
    deriving (Eq, Ord, Show, Read)
  
  data TypeIdentifier = TypeIdentifier Identifier
    deriving (Eq, Ord, Show, Read)
  
  data TypeDefinition
      = TDAlias Type
      | TDRecord [RecordFieldType]
      | TDExtension Identifier [RecordFieldType]
    deriving (Eq, Ord, Show, Read)
  
  data RecordFieldType = RecordFieldType Identifier Type
    deriving (Eq, Ord, Show, Read)
  
  data UnionDefinition = UDEnum Identifier | UDTyped Identifier Type
    deriving (Eq, Ord, Show, Read)
  
  data Type
      = TFunction Type Type
      | TTuple [Type]
      | TList Type
      | TUnit
      | TByName Identifier ExactTypeParam
      | TRecord [RecordFieldType]
      | TParenthesis Type
    deriving (Eq, Ord, Show, Read)
  
  data ExactTypeParam = ExTypeParam [Type] | ExEmptyTypeParam
    deriving (Eq, Ord, Show, Read)
  
  data Literal
      = Char Char
      | String String
      | Integer Integer
      | Float Double
      | UnitValue
    deriving (Eq, Ord, Show, Read)
  
  data Expression
      = EOp Expression Op Expression
      | ERecordField Expression Identifier
      | ETyped Expression Type
      | EApplication Expression [Expression]
      | ENegative Expression
      | ELet BindPattern Expression Expression
      | ELiteral Literal
      | EVariable Identifier
      | EDo Expression
      | EParenthesis Expression
      | ETuple [Expression]
      | EListSequence Expression Expression
      | EListComprehension Expression [Comprehension]
      | EList [Expression]
      | ERecord [RecordFieldAssignment]
      | ERecordUpdate Identifier [RecordFieldAssignment]
      | EIf Expression Expression Expression
      | EIfDo Expression Expression
      | ELambda [Param] Expression
      | EMatch Expression [Alternate]
    deriving (Eq, Ord, Show, Read)
  
  data RecordFieldAssignment
      = RecordFieldAssignment Identifier Expression
    deriving (Eq, Ord, Show, Read)
  
  data Comprehension = Comprehension BindPattern Expression
    deriving (Eq, Ord, Show, Read)
  
  data Alternate = Alternate Pattern Expression
    deriving (Eq, Ord, Show, Read)
  
  data Pattern
      = PVariable Identifier
      | PLiteral Literal
      | PApplication Identifier Pattern
      | PTuple [Pattern]
      | PList [Pattern]
      | PListHead Pattern Pattern
      | PListContains Expression
      | PParenthesis Pattern
      | PWildCard
      | PRecord [RecordPattern]
    deriving (Eq, Ord, Show, Read)
  
  data RecordPattern = RecordPattern Identifier Pattern
    deriving (Eq, Ord, Show, Read)
  
  data BindPattern
      = BParenthesis BindPattern
      | BOp Op [Param]
      | BRecord [RecordBindPattern]
      | BList [BindPattern]
      | BTuple [BindPattern]
      | BListHead BindPattern BindPattern
      | BWildCard
      | BVariable Identifier
      | BFunctionDecl Identifier [Param]
    deriving (Eq, Ord, Show, Read)
  
  data RecordBindPattern = RecordBindPattern Identifier BindPattern
    deriving (Eq, Ord, Show, Read)  