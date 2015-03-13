
PARSING

Uses the arsing library from Hutton (2008) which focuses on using Monadic Parser Combinators

> import Parsing

-----------------------------------------------------------------------------------------------------

PROGRAM IR 

> data Prog                     = Assign Name Expr
> 	   		                        | If Expr Prog
>                                   | IfElse Expr Prog Prog
>                                   | ElseIf Expr Prog [ElseIfCase'] Prog       
> 	   		                        | While Expr Prog           
> 	   		                        | Seqn [Prog]
>                                   | Empty                                     -- Allows blank Prog may be useful 
>                                   | Return (Maybe Expr)                       
>                                   | Func Name [Name] Prog                     -- list of names is names of arguments
>                                   | Main Prog                                 -- gives us the main function to be run
>		                                deriving Show
>
> data ElseIfCase'              = Case Expr Prog                                --used for switch cases and else if 
>                                   deriving Show

EXPRESSION IR

> data Expr  		            =  Val Number 
>                                   | Var Name 
>                                   | ExprApp ArthOp Expr Expr 
>                                   | CompApp CompOp Expr Expr
>                                   | CondApp CondOp Expr Expr 
>                                   | FuncCall Name [Expr]
>                                       deriving Show

For Arthimetic Operations

> data ArthOp    		        =  ADD | SUB | MUL | DIV                        
>			                        deriving (Show, Eq)

For Comparisons

> data CompOp                   = EQU | NEQ | GEQ | LEQ | GRT | LET             
>			                        deriving (Show, Eq)    

For Conditionals, still to be finished 

> data CondOp                   = AND | OR 
>			                        deriving (Show, Eq)  

Numerical Types, currently treating booleans as integers, where 0 is false and 1 is true 

> data Number                   = Integer Int | Double Double
>			                        deriving (Show, Eq)

Name defintion

> type Name                     = String 

-----------------------------------------------------------------------------------------------------

ASSIGNMENT

> evalAssignment                :: String -> Prog 
> evalAssignment xs             =  case (parse assign xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"


Deals with variable name

> assign                        :: Parser Prog
> assign                        = do n <- getName
>                                    do symbol "=" 
>                                       e <- arthExpr 
>                                       return (Assign n e)
>                                     +++ return n

> getName                       :: Parser String
> getName                       =  


-----------------------------------------------------------------------------------------------------

ARTHIMETIC EXPRESSIONS

> evalArthExpr                  :: String -> Expr
> evalArthExpr xs               =  case (parse arthExpr xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

Deals with addition and subtraction, prioritises Subtraction

> arthExpr                      :: Parser Expr
> arthExpr                      =  do s <- subExpr
>                                     do symbol "+"               
>                                        e <- arthExpr
>                                        return (ExprApp ADD s e)
>                                      +++ return s
>
> subExpr                       :: Parser Expr
> subExpr                       =  do t <- term
>                                     do symbol "-"               
>                                        e <- subExpr
>                                        return (ExprApp SUB t e)
>                                      +++ return t
>

Deals with Division and Multiply currently prioritses division, like go does!

> term                          :: Parser Expr
> term                          =  do d <- divTerm
>                                     do symbol "*"
>                                        t <- term
>                                        return (ExprApp MUL d t)
>                                      +++ return d
>
> divTerm                       :: Parser Expr
> divTerm                       =   do f <- factor
>                                      do symbol "/"
>                                         t <- divTerm
>                                         return (ExprApp DIV f t)
>                                       +++ return f
>

> factor                        :: Parser Expr
> factor                        =  do symbol "("
>                                     e <- arthExpr
>                                     symbol ")"
>                                     return e
>                                   +++ do n <- natural
>				           return (Val n)

-----------------------------------------------------------------------------------------------------
