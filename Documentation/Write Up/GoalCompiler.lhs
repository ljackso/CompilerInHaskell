
> module GoalCompiler where

Import the parser and the virtual machine;

> import GoalParser
> import GoalMachine

-----------------------------------------------------------------------------------------------------

MONANDS

Define and import any monads needed here.

Import the writer monad to be used in code generation:

> import Control.Monad.Writer

Define the state monad we want to use to generate unique labels:

> type State 		            =  Label
>
> data ST a 		            =  S (State -> (a, State))
>
> apply        		            :: ST a -> State -> (a,State)
> apply (S f) x 	            =  f x
>
> instance Monad ST where
>    --return 		            :: a -> ST a
>    return x   	            =  S (\s -> (x,s))
>
>    --(>>=)  		            :: ST a -> (a -> ST b) -> ST b
>    st >>= f   	            = S (\s -> let (x,s') = apply st s in apply (f x) s')

Fresh function to update states

> fresh                         :: ST Label
> fresh                         = S (\s -> (s, s+1))

-----------------------------------------------------------------------------------------------------


RUNNER

Parses, then compiles, then executes a .gol program from a file. This is the function you use to
run a .gol file giving the .gol file location as the argument. 

> run f                         = exec (typeChecker (comp (parseGo f)))

-----------------------------------------------------------------------------------------------------
 
 
COMPILER 
 
My code generator 
 
> comp                          :: Prog -> Code
> comp p                        = (fst (apply (execWriterT (compP p)) (0))) 

> compP                         :: Prog -> WriterT Code ST ()
> compP (Seqn ps)               = seqDealerW ps

Functions

> compP (Main p)                = mainDealerW p
> compP (Func n vs p)           = funcDealerW n vs p

Basic commands 

> compP (Assign n e)            = assignDealerW n e
> compP (Return e)              = returnDealerW e
> compP (If e p)                = ifDealerW e p
> compP (IfElse c p1 p2)        = ifElseDealerW c p1 p2
> compP (ElseIf c p1 cs p2)     = elseIfDealerW c p1 cs p2
> compP (While e p3)            = whileDealerW e p3
> compP (VoidFuncCall n es)     = (voidCallDealerW n es)

Deal with channels

> compP (CreateChan n)          = do tell [CHANNEL n]
> compP (PushToChan n e)        = do {tell (expression e); tell [PUSHC n]}

Deal with concurrency commands

> compP (Wait)                  = do tell [WAIT]
> compP (WaitOn n)              = do tell [WAITS n]
> compP (Kill)                  = do tell [KILL]
> compP (WaitChan n)            = do tell [WAITC n] 
> compP (GoCall n es)           = (goCallDealerW n es)

Deal with output commands 

> compP (Show e)                = do {tell (expression e); tell [SHOW]}
> compP (Print p)               = do {tell [PRINT p]}

Some special cases

> compP (Empty)                 = do tell []

Deals with a list of commands 

> seqDealerW                    :: [Prog] -> WriterT Code ST ()
> seqDealerW []                 = do {tell []}
> seqDealerW (p:ps)             = do {compP p; compP (Seqn ps)} 


Deals with generating code in main functions

> mainDealerW                   :: Prog -> WriterT Code ST ()
> mainDealerW p                 = do tell [PUSH (Integer 0), POP "hsg"]
>                                    tell [MAIN]
>                                    compP p 
>                                    tell [PUSHV "hsg", TRICK]
>                                    tell [FEND]      

Deals with generarting code for functions

> funcDealerW                   :: FName -> [Name] -> Prog ->  WriterT Code ST ()
> funcDealerW n vs p            = do tell [FUNC n]
>                                    tell [POP v | v <- (reverse vs)]
>                                    compP p
>                                    tell [FEND] 


Deals with variable assignments

> assignDealerW                 :: Name -> Expr -> WriterT Code ST ()
> assignDealerW n e             = do tell (expression e)
>                                    tell [POP n]

Deals with return statements 

> returnDealerW                 :: Maybe Expr -> WriterT Code ST ()
> returnDealerW e               = case e of
>                                   Just exp    -> do {tell (expression exp); tell [RSTOP]}   
>                                   Nothing     -> do {tell [STOP]} 


Deals with if statements

> ifDealerW                     :: Expr -> Prog -> WriterT Code ST ()
> ifDealerW e p                 = do tell (expression e)
>                                    l <- lift (fresh)
>                                    tell [JUMPZ l]
>                                    compP p
>                                    tell [LABEL l]


Deals with if else statements 

> ifElseDealerW                 :: Expr -> Prog -> Prog -> WriterT Code ST ()
> ifElseDealerW e p1 p2         = do l1 <- lift fresh
>                                    l2 <- lift fresh
>                                    tell (expression e)
>                                    tell [JUMPZ l1]
>                                    compP p1
>                                    tell [JUMP l2]
>                                    tell [LABEL l1]
>                                    compP p2
>                                    tell [LABEL l2]       
                                       
Deals with else if expressions 

> elseIfDealerW                 :: Expr -> Prog -> [ElseIfCase'] -> Prog -> WriterT Code ST () 
> elseIfDealerW e p1 ps p2      = do l1 <- lift fresh 
>                                    l2 <- lift fresh
>                                    el <- lift fresh
>                                    tell (expression e)
>                                    tell [JUMPZ l1]
>                                    compP p1
>                                    tell [JUMP el, LABEL l1]
>                                    elseIfCaseDealer ps el
>                                    compP p2
>                                    tell [LABEL el]

> elseIfCaseDealer              :: [ElseIfCase'] -> Label ->  WriterT Code ST ()
> elseIfCaseDealer [] el        = do tell []
> elseIfCaseDealer (c:cs) el    = do elseIfCaseDealer' c el
>                                    elseIfCaseDealer cs el

> elseIfCaseDealer'             :: ElseIfCase' -> Label ->  WriterT Code ST ()
> elseIfCaseDealer' (Case c p1) el  
>                               = do l <- lift fresh 
>                                    tell (expression c)
>                                    tell [JUMPZ l]
>                                    compP p1
>                                    tell [JUMP el, LABEL l]
   
Deals with while loops

> whileDealerW                  :: Expr -> Prog -> WriterT Code ST ()
> whileDealerW e p              = do l1 <- lift fresh
>                                    l2 <- lift fresh
>                                    tell [LABEL l1]
>                                    tell (expression e)
>                                    tell [JUMPZ l2]
>                                    compP  p
>                                    tell [JUMP l1]
>                                    tell [LABEL l2]   

Deal with creating new go subroutine

> goCallDealerW                 :: Name -> [Expr] -> WriterT Code ST ()
> goCallDealerW n es            = do tell (multExpression es)
>                                    tell [GO n]

Deal with void function calls

> voidCallDealerW               :: Name -> [Expr] -> WriterT Code ST ()
> voidCallDealerW n []          = do tell [VCALL n]
> voidCallDealerW n es          = do tell (multExpression es)
>                                    tell [VCALL n]   


EXPRESSIONS 

These functions handle generating code for expressions

> expression                    :: Expr -> Code
> expression (FuncCall n es)    = (callDealer n es)
> expression (Val i)            = [PUSH i]
> expression (Valb b)           = [PUSH b]
> expression (Var v)            = [PUSHV v]
> expression (ExprApp o x y)    = expression x ++ expression y ++ [DO o] 
> expression (CompApp o x y)    = expression x ++ expression y ++ [COMP o]
> expression (CondApp o x y)    = expression x ++ expression y ++ [COND o]
> expression (PopFromChan n)    = [POPC n]

Deal with a list of Expr

> multExpression                :: [Expr] -> Code
> multExpression []             = []
> multExpression (e:es)         = expression e ++ multExpression es           


Deals with function call puts result ontop of the stack if there is a return statement

> callDealer                    :: Name -> [Expr] -> Code
> callDealer n []               = [CALL n] 
> callDealer n (es)             = multExpression es ++ [CALL n] 




