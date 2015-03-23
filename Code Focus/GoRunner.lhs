
> module GoRunner where

> import Data.List
> import Data.List.Split
> import GoParser


COMPILER & EXECUTOR

This is where I focus on code genration and creating a virtual machine to, compile and execute my code.
I treat my input as a high level representation of Go as a data structure that I get from GoParser. 

-----------------------------------------------------------------------------------------------------

MONANDS

Define any monads needed here

State monad:

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

Parses, then compiles, then executes a go program from a file. 

> run f                         = exec (comp (parseGo f))

-----------------------------------------------------------------------------------------------------

COMPILER

Converts program into machine code

> comp                          :: Prog -> Code
> comp p                        =  fst (apply (comp' p) (0))


Actually compiles to machine code  

> comp'                         :: Prog -> ST Code
> comp' (Main p1)               = (mainDealer p1)
> comp' (Func n vs p1)          = (functionDealer n vs p1) 
> comp' (Seqn cs)               = (sequenceDealer cs)
> comp' (Assign n e)            = return (expression e ++ [POP n])
> comp' (If c p)				= (ifDealer c p)
> comp' (IfElse c p1 p2)        = (ifElseDealer c p1 p2)
> comp' (ElseIf c p1 cs p2)     = (elseIfDealer c p1 cs p2)
> comp' (While e p3)            = (whileDealer e p3)
> comp' (Return e)              = (returnDealer e)          
> comp' (Empty)                 = return []  

Deals with Expr

> expression                    :: Expr -> Code
> expression (FuncCall n es)    = (callDealer n es)
> expression (Val i)            = [PUSH i]
> expression (Var v)            = [PUSHV v]
> expression (ExprApp o x y)    = expression x ++ expression y ++ [DO o] 
> expression (CompApp o x y)    = expression x ++ expression y ++ [COMP o]


Deal with a list of Expr

> multExpression                :: [Expr] -> Code
> multExpression []             = []
> multExpression (e:es)         = expression e ++ multExpression es             


Deals with if 

> ifDealer                      :: Expr -> Prog -> ST Code 
> ifDealer c p1                 =   do  l       <- fresh
>                                       p1code  <- comp' p1
>                                       return (expression c ++ [JUMPZ l] ++ p1code ++ [LABEL l])   

Deals with if else

> ifElseDealer                  :: Expr -> Prog -> Prog -> ST Code
> ifElseDealer c p1 p2          =   do  l1 		<- fresh
>                                       l2      <- fresh
>                                       p1code 	<- comp' p1
>                                       p2code 	<- comp' p2
>                                       return (expression c ++ [JUMPZ l1] ++ p1code ++ [JUMP l2] ++ [LABEL l1] ++ p2code ++ [LABEL l2])

Deals with else if 

> elseIfDealer                  :: Expr -> Prog -> [ElseIfCase'] -> Prog -> ST Code 
> elseIfDealer  c p1 cs p2      =   do  l1      <- fresh
>                                       l2      <- fresh
>                                       el      <- fresh
>                                       p1code  <- comp' p1
>                                       p2code  <- comp' p2
>                                       csCode  <- listCaseDealer cs el      
>                                       return (expression c ++ [JUMPZ l1] ++ p1code ++ [JUMP el] ++ [LABEL l1] ++ csCode ++ p2code ++ [LABEL el])

Deals with a list of else if cases

> listCaseDealer                :: [ElseIfCase'] -> Label -> ST Code
> listCaseDealer [] el          = return []  
> listCaseDealer (c:cs) el      =   do  cCode   <- (caseDealer c el)
>                                       csCode  <- (listCaseDealer cs el) 
>                                       return (cCode ++ csCode)

Deals with an individual else if case

> caseDealer                    :: ElseIfCase' -> Label -> ST Code
> caseDealer (Case c p1) el     =   do  l 		<- fresh
>                                       p1code 	<- comp' p1
>                                       return (expression c ++ [JUMPZ l] ++ p1code ++ [JUMP el] ++ [LABEL l])                          


Deals with While    

> whileDealer                   :: Expr -> Prog -> ST Code
> whileDealer e p               =   do  l1      <- fresh
>                                       l2      <- fresh
>                                       pcode   <- comp' p
>                                       return ([LABEL l1] ++ expression e ++ [JUMPZ l2] ++ pcode ++ [JUMP l1] ++ [LABEL l2])

Deals with Return 

> returnDealer					:: Maybe Expr -> ST Code
> returnDealer e				=   case e of
>                                       Just exp    -> return (expression exp ++ [RSTOP])    
>                                       Nothing     -> return ([STOP])       

Deals With Sequences

> sequenceDealer                :: [Prog] -> ST Code 
> sequenceDealer []             = return []
> sequenceDealer (c:cs)         =   do  head    <- comp' c
>                                       tail    <- comp' (Seqn cs)
>                                       return (head ++ tail)  

Deals with Functions, assumes that ever function contains a return will be done at parsing level

> functionDealer                :: Name -> [Name] -> Prog ->  ST Code
> functionDealer n vs p1        =   do  p1code  <- comp' p1
>                                       return ([FUNC n] ++ [ POP v | v <-(reverse vs)] ++ p1code ++ [FEND])

Deals with function call puts whatever it is ontop of the stack if there is a return statement

> callDealer                    :: Name -> [Expr] -> Code
> callDealer n []               = [CALL n 0] 
> callDealer n (es)             = multExpression es ++ [CALL n (length es)] 

Deals with setting up the Main function and all the code to be run

> mainDealer                    :: Prog -> ST Code
> mainDealer p1                 =   do  p1Code  <- comp' p1 
>                                       return ([MAIN] ++ p1Code ++ [END]) 


-----------------------------------------------------------------------------------------------------

LOW LEVEL

Now we define what we ant our high level code to compile down too, for now I define my own low
level code and create and executer for it. 

This section can either pre repleaced or used as an intermediatary for a more useful low level 
language.

This can be represented as a mini virtual machine, we will use the same vm as was used in AFP so
as to focus n high level interpritation, look to improve this later. 

> type Stack 		            = [Number]
>
> type Mem 		                = [(Name, Number)]
>   
> type Code  		            = [Inst]
> 
> data Inst  		            =  PUSH Number
>          		                    | PUSHV Name
>          		                    | POP Name
>          		                    | DO ArthOp
>          		                    | COMP CompOp
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


> instantiateMemory             :: Mem 
> instantiateMemory             = [("", Integer 0) | x <- [1..10]]



-----------------------------------------------------------------------------------------------------
 
 EXECUTER 
 
Almost entirely from AFP at this stage a simple compiler for executing the code we have will update later on

This takes a list of machine code instructions and executes them.

> exec                          :: Code -> Maybe Number
> exec c                        = exec' c c 0 [] instantiateMemory True


This is the function that actually executes the code, c is the intial code, ec is what you're executing

Variable Explanations;

c = all the code
ec = the current batch of code being executed
pc = program counter
s = stack
m = memory 
g = isGlobal, says whether you are workin within a function or working globally, used for memory mangement

> exec'                         :: Code -> Code ->  Int -> Stack -> Mem -> Bool -> Maybe Number
> exec' c ec pc s m g           =   case ec !! pc of 
>                                       MAIN        -> exec' c ec (pc+1) s m False
>                                       FUNC n      -> exec' c ec (pc+1) s m g       
>                                       PUSH n      -> exec' c ec (pc+1) (push n s) m g
>                                       PUSHV v     -> exec' c ec (pc+1) (pushv v m s g) m g
>                                       POP v       -> exec' c ec (pc+1) (pop s) (assignVariable v (head s) m g) g
>                                       DO o        -> exec' c ec (pc+1) (operation o s) m g
>                                       COMP o      -> exec' c ec (pc+1) (comparison o s) m g
>                                       LABEL l     -> exec' c ec (pc+1) s m g
>                                       JUMP l      -> exec' c ec (jump c l) s m g
>                                       JUMPZ l     -> exec' c ec (jumpz c s l pc) (pop s) m g
>                                       (CALL n a)  -> (handleCall c ec pc s m n a g)  
>                                       RSTOP       -> Just (head s)
>                                       STOP        -> Nothing
>                                       END         -> Nothing 
>                                       FEND        -> exec' c ec (pc+1) s m g                  --- happens for void functions     
         
Returns the stack, used for function calls to managing stack frames and make sure variables,
don't get lost. Does the same as exec' but instead returns the stack.          
         
> stackExec                     :: Code -> Stack
> stackExec c                   = stackExec' c c 0 [] [] True               

> stackExec'                    :: Code -> Code -> Int -> Stack -> Mem -> Bool -> Stack
> stackExec' c ec pc s m g      =   case ec !! pc of 
>                                       MAIN        -> stackExec' c ec (pc+1) s m False 
>                                       FUNC n      -> stackExec' c ec (pc+1) s m g        
>                                       PUSH n      -> stackExec' c ec (pc+1) (push n s) m g
>                                       PUSHV v     -> stackExec' c ec (pc+1) (pushv v m s g) m g
>                                       POP v       -> stackExec' c ec (pc+1) (pop s) (assignVariable v (head s) m g) g
>                                       DO o        -> stackExec' c ec (pc+1) (operation o s) m g
>                                       COMP o      -> stackExec' c ec (pc+1) (comparison o s) m g 
>                                       LABEL l     -> stackExec' c ec (pc+1) s m g
>                                       JUMP l      -> stackExec' c ec (jump ec l) s m g
>                                       JUMPZ l     -> stackExec' c ec (jumpz ec s l pc) (pop s) m g
>                                       (CALL n a)  -> (handleCallReStack c ec pc s m n a g)  
>                                       RSTOP       -> s
>                                       STOP        -> s
>                                       END         -> s 
>                                       FEND        -> stackExec' c ec (pc+1) s m g
         


STACK FUCNTIONS

This is a series of functions that handle the stack


push onto top of stack an integer

> push                          :: Number -> Stack -> Stack
> push (n) s                    =  [n] ++ s


push variable onto top of the stack

> pushv                         :: Name ->  Mem -> Stack -> Bool -> Stack
> pushv v m s g                 =  push (getVariable v m g) s


pop an integer from the stack and places it into memory under that variable name

> pop                           :: Stack -> Stack
> pop s                         = tail s


Perform an operation on the first two things of the stack and leave the result on top

> operation                     :: ArthOp -> Stack -> Stack
> operation o s                 =   case o of 
>                                       ADD -> push (addNumbers v2 v1) ns 
>                                       SUB -> push (subNumbers v2 v1) ns
>                                       MUL -> push (mulNumbers v2 v1) ns 
>                                       DIV -> push (divNumbers v2 v1) ns
>                                   where
>                                       v1 = head s
>                                       v2 = head (tail s)
>                                       ns = (pop (pop s))

Perform a comparison on the first two things on the stack (leave a 1 on top if true and 0 if false)

> comparison                    ::  CompOp -> Stack -> Stack
> comparison o s                =   push (comparison' o v1 v2) ns 
>                                       where
>                                           v2 = (head s)
>                                           v1 = (head (tail s))
>                                           ns = (pop (pop s))
>
> comparison'                   :: CompOp -> Number -> Number -> Number 
> comparison' o v1 v2           =   case o of
>                                       EQU		-> if isNumEqu v1 v2  then t else f    
>                                       NEQ     -> if isNumNeq v1 v2  then f else t
>                                       GEQ     -> if isNumGeq v1 v2  then t else f
>                                       LEQ     -> if isNumLeq v1 v2  then t else f
>                                       GRT     -> if isNumGrt v1 v2  then t else f
>                                       LET     -> if isNumLet v1 v2  then t else f                                       
>                                   where 
>                                       t = Integer 1
>                                       f = Integer 0
                                                           

pops the head if it is 0 if not it leaves it be 

> popz                          :: Stack -> Stack
> popz s                
>                               | top == 0          = pop s
>                               | otherwise         = s
>                                   where
>                                       top = getInt (head s)                


Multiple pop, pops the required number of things

> multiplePop                   :: Stack -> Int -> Stack
> multiplePop ss 0              = ss
> multiplePop ss n              = multiplePop (pop ss) (n-1)   
 

MEMORY FUNCTIONS

Functions that deal with memory


This function assigns an integer to a variable

> assignVariable                :: Name -> Number -> Mem -> Bool -> Mem
> assignVariable v n m g        = if g then [(v, n)] ++ (deleteVariable v m)
>                                 else localAssign v n m 


Get the variable from memory, if not assigned already just assume that variable is 0 

> getVariable                   :: Name -> Mem -> Bool -> Number
> getVariable v ms g            
>                               | null memV     = Integer 0 
>                               | otherwise     = snd (head memV)
>                                   where 
>                                       memV = [ m | m <- ms, fst m == v] 

Checks that the variable isn't being reassigned and if it is deletes that variable

> deleteVariable                :: Name -> Mem -> Mem
> deleteVariable v ms           = [ m | m <- ms, fst m /= v] 

> localAssign                   :: Name -> Number -> Mem -> Mem  
> localAssign v n ms            = if (isGlobal v ms) then [(v,n)] ++ deleteVariable v ms
>                                 else deleteVariable v ms ++ [(v,n)]  

> isGlobal                      :: Name -> Mem -> Bool
> isGlobal v (("",(Integer 0)):xs) 
>                               = False
> isGlobal v (x:xs)             = if (fst x) == v then True
>                                 else (isGlobal v xs) 
 
 
JUMP FUNCTIONS

All the functions that deal with jumping arounbd our code

> jump                          ::  Code -> Int -> Int
> jump cs l                     =   case elemIndex True [ isLabel c l | c <- cs] of 
>                                       Just n      -> n
>                                       Nothing     -> 0     --Should never happen


deals with jumpz

> jumpz                         :: Code -> Stack -> Int -> Int -> Int
> jumpz c s l pc
>                               | head s == (Integer 0)     = jump c l
>                               | otherwise                 = (pc + 1)

returns true if is the label we are looking for

> isLabel                       :: Inst -> Int -> Bool
> isLabel c l                   =   case c of 
>                                       LABEL m         ->  (m == l) 
>                                       otherwise       ->  False  

stops the program, just returns memory at the moment 

> halt 							:: Mem -> Mem   
> halt m						= m 


CALL Functions

These functions deal with function calls, if returns something put ontop off stack otherwise do 

> handleCall                    :: Code -> Code -> Int -> Stack -> Mem -> Name -> Int -> Bool -> Maybe Number
> handleCall c ec pc s m n a g  =   exec' c ec (pc+1) fs m g   
>                                   where
>                                       fs      = stackExec' c fCode 0 s gm g
>                                       fCode   = (getFunction c n [] False False)
>                                       gm      = getGlobalMemmory m

                      
Returns Stack, used for nested and recursive function calls

> handleCallReStack             :: Code -> Code-> Int -> Stack -> Mem -> Name -> Int -> Bool -> Stack
> handleCallReStack c ec pc s m n a g
>                               = stackExec' c ec (pc+1) (fRun) m g
>                                   where
>                                       fRun    = stackExec' c fCode 0 s gm g
>                                       fCode   = (getFunction c n [] False False)
>                                       gm      = getGlobalMemmory m   
> 

  
Searches through code and returns a function's code, ct signifies if is currently cutting the code

> getFunction                   :: Code -> Name -> Code -> Bool -> Bool -> Code 
> getFunction (c:cs) n fs ct mt =   if ct then
>                                       if (c == FEND) then (reverse (c:fs))
>                                       else getFunction cs n (c:fs) True mt                                           
>                                   else  
>                                       if (c == (FUNC n)) then getFunction cs n (c:fs) True mt
>                                       else getFunction cs n fs False mt 
>                                   

--------------------------------------------------------------------------------

MEMORY MANGEMENT

These series of function deal with memory mangement and handling global and local variables

Memory is split into 2 sections the first 20% of memory is for global variables and the rest is for local memory!  


> getGlobalMemmory              :: Mem -> Mem
> getGlobalMemmory  ms          =  getGlobalMemmory' ms [] ++ instantiateMemory
>
> getGlobalMemmory'             :: Mem -> Mem -> Mem 
> getGlobalMemmory' (m:ms) gs   = if (fst m) == "" then reverse gs
>                                 else getGlobalMemmory' ms (m:gs)   

--------------------------------------------------------------------------------


TYPE FUNCTIONS 

> getInt                        :: Number -> Int
> getInt (Integer n)            = n

> getDouble                     :: Number -> Double
> getDouble (Double n)          = n 

> isInt                         :: Number -> Bool
> isInt (Integer n)             = True
> isInt _                       = False 

> isDouble                      :: Number -> Bool
> isDouble (Double n)           = True
> isDouble _                    = False

Comparison Operations

> isNumEqu                      :: Number -> Number -> Bool
> isNumEqu x y                  | (isInt x)         = (getInt x) == (getInt y) 
>                               | otherwise         = (getDouble x) == (getDouble y)

> isNumNeq                      :: Number -> Number -> Bool
> isNumNeq x y                  | (isInt x)         = (getInt x) /= (getInt y) 
>                               | otherwise         = (getDouble x) /= (getDouble y)

> isNumGeq                      :: Number -> Number -> Bool
> isNumGeq x y                  | (isInt x)         = (getInt x) >= (getInt y) 
>                               | otherwise         = (getDouble x) >= (getDouble y)

> isNumLeq                      :: Number -> Number -> Bool
> isNumLeq x y                  | (isInt x)         = (getInt x) <= (getInt y) 
>                               | otherwise         = (getDouble x) <= (getDouble y)

> isNumGrt                      :: Number -> Number -> Bool
> isNumGrt x y                  | (isInt x)         = (getInt x) > (getInt y) 
>                               | otherwise         = (getDouble x) > (getDouble y)

> isNumLet                      :: Number -> Number -> Bool
> isNumLet x y                  | (isInt x)         = (getInt x) < (getInt y) 
>                               | otherwise         = (getDouble x) < (getDouble y)

Arthimetic opperations

> addNumbers                    :: Number -> Number -> Number   -- (x + y)
> addNumbers x y                =   if (isInt x) then Integer ((getInt x) + (getInt y)) 
>                                   else Double ((getDouble x) + (getDouble y))

> subNumbers                    :: Number -> Number -> Number   -- (x - y)
> subNumbers x y                =   if (isInt x) then Integer ((getInt x) - (getInt y))
>                                   else Double ((getDouble x) - (getDouble y)) 

Need to be carefull with divide because of how Go deals with int divison compared to haskell

> divNumbers			        :: Number -> Number -> Number   -- (x / y)
> divNumbers x y                =   if (isInt x) then (divideInt (getInt x) (getInt y))
>                                   else (divideDouble (getDouble x) (getDouble y))

> divideInt                     :: Int -> Int -> Number 
> divideInt x y                 = Integer (quot x y) 


> divideDouble                  :: Double -> Double -> Number
> divideDouble x y              = Double (x / y)

> mulNumbers                    :: Number -> Number -> Number   -- (x * y)
> mulNumbers x y                =   if (isInt x) then Integer ((getInt x) * (getInt y))
>                                   else Double ((getDouble x) * (getDouble y))

--------------------------------------------------------------------------------
