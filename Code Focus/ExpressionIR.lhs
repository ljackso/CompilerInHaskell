
> module ExpressionIR where 

EXPRESSION IR

This is taken from Cooper & Torczon (2012) and used to help distinguish betwen arthemtic and boolean expressions

> data Expr                     = And Expr AndTerm 
>                                   | OnlyE AndTerm                                   -- Expr AND AndTerm
>                                   | FuncCall Name [Expr]
>			                            deriving (Show, Eq)

> data AndTerm                  = Or AndTerm RelExpr 
>                                   | OnlyA RelExpr                                               -- AndTerm OR RelExpr 
>			                            deriving (Show, Eq)
> 
> data CondOp                   = AND | OR
>			                        deriving (Show, Eq)

> data RelExpr                  = RelApp RelOp RelExpr NumExpr 
>                                   | OnlyR NumExpr 
>			                            deriving (Show, Eq)
>   
> data RelOp                    = EQU | NEQ | GEQ | LEQ | GRT | LET             
>			                        deriving (Show, Eq)

> data NumExpr                  =  Sub NumExpr Term 
>                                   | Add NumExpr Term
>                                   | OnlyN Term   
>			                            deriving (Show, Eq)

> data Term                     = Mul Term Factor
>                                   | Div Term Factor
>                                   | OnlyT Factor
>			                            deriving (Show, Eq)

> data Factor                   = Val Number | Var Name 
>			                        deriving (Show, Eq)

> data ArthOp    		        =  ADD | SUB | MUL | DIV                        -- Arithmetic operations
>			                        deriving (Show, Eq)

> data Number                   = Integer Int | Double Double
>			                        deriving (Show, Eq)

> type Name                     = String 

FOR TESTING 

> var                           :: Name -> Expr
> var n                         = OnlyE (OnlyA (OnlyR (OnlyN (OnlyT (Var n)))))

> val                           :: Number -> Expr 
> val v                         = OnlyE (OnlyA (OnlyR (OnlyN (OnlyT (Val v)))))   


Deals with Expr

> expression                    :: Expr -> Code
> expression (And e a)          = expression e ++ andTerm a ++ [COND AND]
> expression (FuncCall n es)    = []                                              -- (callDealer n es)
> expression (OnlyE a)          = andTerm a 

> andTerm                       :: AndTerm -> Code 
> andTerm (Or a r)              = andTerm a ++ relExpression r ++ [COND OR]
> andTerm (OnlyA r)             = relExpression r 

> relExpression                 :: RelExpr -> Code
> relExpression (RelApp o r n)  = relExpression r ++ numTerm n ++ [COMP o]
> relExpression (OnlyR r)       = numTerm r

> numTerm                       :: NumExpr -> Code
> numTerm (Sub n t)             = numTerm n ++ term t ++ [DO SUB]
> numTerm (Add n t)             = numTerm n ++ term t ++ [DO ADD]
> numTerm (OnlyN n)             = term n

> term                          :: Term -> Code
> term (Mul t f)                = term t ++ factor f ++ [DO MUL]
> term (Div t f)                = term t ++ factor f ++ [DO MUL]
> term (OnlyT t)                = factor t

> factor                        :: Factor -> Code
> factor (Val n)                = [PUSH n]
> factor (Var v)                = [PUSHV v]


TARGET CODE 

> type Stack 		            =  [Number]
>
> type Mem 		                =  [(Name, Number)]		
>   
> type Code  		            =  [Inst]
> 
> data Inst  		            =  PUSH Number
>          		                    | PUSHV Name
>          		                    | POP Name
>          		                    | DO ArthOp
>          		                    | COMP RelOp
>                                   | COND CondOp 
>                                   | JUMP Label
>          		                    | JUMPZ Label
>          		                    | LABEL Label
>                                   | FUNC Name 
>                                   | FEND
>                                   | CALL Name Int                       
>                                   | STOP                      --Used for returning nothing
>                                   | RSTOP                     --Used to return a variable from a function
>                                   | MAIN
>                                   | END                       --Signifies the end of the main function         
>		                                deriving (Show, Eq)
> 
> type Label 		            =  Int