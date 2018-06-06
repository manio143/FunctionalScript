# Features for v2 of Functional Script

#### Misc
- `import "file"` to allow modular programs (as well as a standard library)

- Sort of type classes
    - `interface (id) :: (type)`
    - `impl (id) for (type) -> let (bp) = (expr)`
    
#### Types
- Allow extension of a type rather than identifier thus allowing to extend generic records
- Allow for annotating operators `@ (op) :: type`
- Higher kinded types of some sort

#### Expressions
- Allow record extending of expressions rather than identifiers
- ~~MonadicExpression - `monad(return, bind) { monad-expr }`
    - `let! (bp) = (expr) [in]`
    - `do! (expr) [in]`~~
- Allow using operators as values `let x = (+) <=> let x = \a b -> a + b`
