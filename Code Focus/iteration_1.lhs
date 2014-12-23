
First define the program features we want to compile these are amore formal definitons of Go's features,
It is what we want our Program to be represented by once we have manged to parse it.

TODO : Add support fot elseif statements

> data Prog  		    =  Assign Name Expr
> 	   		                | If Cond Prog
>                           | IfElse Cond Prog Prog
> 	   		                | While Cond Prog
> 	   		                | Seqn [Prog]
>                           | Empty         -- Allows blank sequences. may be useful 
>                           | Return Expr
>		                    deriving Show
>

How variables are represented and used.

> data Expr  		    =  Val Type Number | Var Name | ExprApp ArthOp Expr Expr
>			                deriving Show
>
> data ArthOp    		=  ADD | SUB | MUL | DIV        -- Arithmetic operations
>			                deriving Show
>
> type Type             = INT | DOUBLE | BOOL           --Let booleans be represented by number 0 is false 1 >= is true
>
> type Name  		    =  String
>

How conditionals are expressed

TODO : and suppost for multiple conditonals eg implement and and or 

> data Cond             = CompApp CompOp Expr Expr 
>
> data CompOp           = EQ | NEQ | GEQ | LEG | GT 