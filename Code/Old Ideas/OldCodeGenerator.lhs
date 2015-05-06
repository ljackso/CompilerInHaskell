
COMPILER

Converts program into machine code

> comp                          :: Prog -> Code
> comp p                        =  fst (apply (comp' p) (0))

Actually compiles to machine code  

> comp'                         :: Prog -> ST Code
> comp' (Seqn cs)               = (sequenceDealer cs)
> comp' (Main p1)               = (mainDealer p1)
> comp' (Func n vs p1)          = (functionDealer n vs p1) 
> comp' (Assign n e)            = return (expression e ++ [POP n])
> comp' (If c p)				= (ifDealer c p)
> comp' (IfElse c p1 p2)        = (ifElseDealer c p1 p2)
> comp' (ElseIf c p1 cs p2)     = (elseIfDealer c p1 cs p2)
> comp' (While e p3)            = (whileDealer e p3)
> comp' (Return e)              = (returnDealer e)          
> comp' (Empty)                 = return [] 
> comp' (Show e)                = return (expression e ++ [SHOW])
> comp' (Print p)               = return ([PRINT p])
> comp' (CreateChan n)          = return ([CHANNEL n])
> comp' (PushToChan n e)        = return (expression e ++ [PUSHC n])
> comp' (Wait)                  = return [WAIT]
> comp' (WaitOn n)              = return [WAITS n]
> comp' (Kill)                  = return [KILL]
> comp' (WaitChan n)            = return [WAITC n] 
> comp' (GoCall n es)           = (goCallDealer n es)
> comp' (VoidFuncCall n es)     = (voidCallDealer n es)



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

> functionDealer                :: FName -> [Name] -> Prog ->  ST Code
> functionDealer n vs p1        =   do  p1code  <- comp' p1
>                                       return ([FUNC n] ++ [ POP v | v <-(reverse vs)] ++ p1code ++ [FEND])



Deals with setting up the Main function and all the code to be run. You need to start and end by creating and 
then calling a random variable name to stop Haskells lazy evaluation scheme missing out void functions. 

> mainDealer                    :: Prog -> ST Code
> mainDealer p1                 =   do  p1Code  <- comp' p1 
>                                       return ([PUSH (Integer 0)] ++ [POP "hsglennert"] ++ [MAIN] ++ p1Code ++ [PUSHV "hsglennert"] ++ [TRICK] ++ [FEND]) 

Deal with creating a new go subroutine

> goCallDealer                  :: Name -> [Expr] -> ST Code
> goCallDealer n es             = return (multExpression es ++ [GO n])

Deal with void function calls

> voidCallDealer                :: Name -> [Expr] -> ST Code
> voidCallDealer n []           = return ([VCALL n]) 
> voidCallDealer n (es)         = return (multExpression es ++ [VCALL n])  

