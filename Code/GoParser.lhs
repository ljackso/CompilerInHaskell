
PARSING

Uses the Parsing library from Hutton (2008) which focuses on using Monadic Parser Combinators

> module GoParser where

> import Parsing
> import System.IO.Unsafe


-----------------------------------------------------------------------------------------------------

PROGRAM IR 

> data Prog                     = CreateChan Name 
>                                   | PushToChan Name Expr
>                                   | Assign Name Expr
>                                   | Show Expr
>                                   | Print String
> 	   		                        | If Expr Prog
>                                   | IfElse Expr Prog Prog
>                                   | ElseIf Expr Prog [ElseIfCase'] Prog       
> 	   		                        | While Expr Prog           
> 	   		                        | Seqn [Prog]
>                                   | Empty                                     -- Allows blank Prog may be useful 
>                                   | Return (Maybe Expr)                       
>                                   | Func FName [Name] Prog                     -- list of names is names of arguments
>                                   | Main Prog                                 -- gives us the main function to be run
>                                   | VoidFuncCall Name [Expr]
>                                   | GoCall Name [Expr]                                                                    
>                                   | Wait 
>                                   | Kill
>		                                deriving Show
>
> data ElseIfCase'              = Case Expr Prog                                --used for switch cases and else if 
>                                   deriving Show

EXPRESSION IR

> data Expr  		            =  Val Number
>                                   | Valb Number 
>                                   | Var Name 
>                                   | ExprApp ArthOp Expr Expr 
>                                   | CompApp CompOp Expr Expr
>                                   | CondApp CondOp Expr Expr 
>                                   | FuncCall Name [Expr]
>                                   | PopFromChan Name
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


Type system 
  

Numerical Types, currently treating booleans as integers, where 0 is false and 1 is true 

> data Number                   = Integer Int | Double Double | Bool Int 
>			                        deriving (Show, Eq)

Name defintion

> type Name                     = String 

Function Names, they must have return type aswell

> type FName                    = (Name, FType)

> data FType                    = VOID | INT 
>                                   deriving (Show, Eq)


-----------------------------------------------------------------------------------------------------

PROGRAM PARSE

This code will parse a sequence of instructions, for now assume all instructions need a ";" at the end,
athough this is optional in Go.

Evaluates a whole program 

> parseGo                       :: String -> Prog  
> parseGo fs                    =   if validFile fs then 
>                                       unsafePerformIO( do go <- readFile fs
>                                                           return (evalProg go))
>                                   else error "Unvalid file, ust be a .gol file!"  

> validFile                     :: String -> Bool
> validFile s                   = reverse (take 4 (reverse s)) == ".gol"  

> evalProg                      :: String -> Prog 
> evalProg xs                   = case (parse parseProg xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input" ++ out)
>                                     []        -> error "invalid input at function level"

> parseProg                     :: Parser Prog
> parseProg                     = do space
>                                    a <- multipleAssign
>                                    space
>                                    m <- parseMain
>                                    symbol ";"
>                                    space
>                                    fs <- parseFuncs
>                                    space
>                                    return (Seqn (a ++ (m:fs)))      

Parse Multiple Functions

> parseFuncs                    :: Parser [Prog] 
> parseFuncs                    = do f <- parseFunc
>                                    do symbol ";"
>                                       fs <- parseFuncs
>                                       return (f:fs)
>                                     +++ do symbol ";" 
>                                            return [f]   
>                                  +++ do string ""
>                                         return []   


Changed so main can return an int unlike in Go

> parseMain                     :: Parser Prog                  
> parseMain                     = do string "func main"
>                                    symbol "()"
>                                    symbol "{"
>                                    p <- parseCommands
>                                    symbol "}"                                    
>                                    return (Main p)

> parseFunc                     :: Parser Prog
> parseFunc                     = do string "func"
>                                    n <- identifier
>                                    symbol "("
>                                    ar <- arguments
>                                    symbol ")"
>                                    t <- getType 
>                                    symbol "{"
>                                    p1 <- parseCommands
>                                    symbol "}"
>                                    return (Func (n,t) ar p1)  


> getType                       :: Parser FType
> getType                       = do string "int"
>                                    return (INT)
>                                  +++ do string "bool"
>                                         return (INT)
>                                  +++ do string ""
>                                         return (VOID)

> arguments                     :: Parser [Name] 
> arguments                     = do a <- getArgument
>                                    do symbol ","
>                                       ar <- arguments
>                                       return (a:ar)
>                                     +++ return [a]                                   
>                                  +++ do string ""
>                                         return [] 
 
>
> getArgument                   :: Parser Name
> getArgument                   = do a <- identifier
>                                    do string "int" 
>                                       return a
>                                     +++ do string "bool"
>                                            return a 

Evaluates a list of commands you might find in a functions

> evalCommands                  :: String -> Prog
> evalCommands xs               = case (parse parseCommands xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input command in function"

> parseCommands                 :: Parser Prog
> parseCommands                 = do p1 <- parseCommand
>                                    do symbol ";"
>                                       p2 <- parseCommands
>                                       return (Seqn [ p1,p2 ])
>                                     +++ do symbol ";"
>                                            return p1 
>                                    

> parseCommand                  :: Parser Prog 
> parseCommand                  =  do a <- assign
>                                     return a  
>                                   +++ do r <- return'
>                                          return r
>                                   +++ do s <- parseShow
>                                          return s
>                                   +++ do p <- parsePrint
>                                          return p  
>                                   +++ do fe <- elseIfParse
>                                          return fe
>                                   +++ do e <- ifElseParse
>                                          return e 
>                                   +++ do f <- ifParse
>                                          return f
>                                   +++ do fr <- forParse
>                                          return fr
>                                   +++ do w <- whileParse
>                                          return w
>                                   +++ do c <- parseChanelCreation
>                                          return c
>                                   +++ do cp <- parseChanelPush
>                                          return cp
>                                   +++ do g <- parseGoCall
>                                          return g 
>                                   +++ do gc <- parseProcessCommand
>                                          return gc
>                                   +++ do v <- parseVoidFun
>                                          return v   
  

-----------------------------------------------------------------------------------------------------

FOR LOOPS

> evalFor                       :: String -> Prog              
> evalFor xs                    = case (parse forParse xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"
>
> forParse                      :: Parser Prog
> forParse                      = do string "for"
>                                    symbol "("
>                                    a1 <- assign
>                                    symbol ";"
>                                    c <- compExpr
>                                    symbol ";"
>                                    a2 <- assign
>                                    symbol ")"
>                                    symbol "{"
>                                    p <- parseCommands
>                                    symbol "}"       
>                                    return (Seqn [ a1, While c (Seqn [ p, a2])])     
  
-----------------------------------------------------------------------------------------------------


WHILE STATEMENTS

> evalWhile                     :: String -> Prog 
> evalWhile xs                  = case (parse whileParse xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> whileParse                    :: Parser Prog 
> whileParse                    = do string "for"
>                                    symbol "(" 
>                                    e <- compExpr    
>                                    symbol ")"
>                                    symbol "{"
>                                    r <- parseCommands
>                                    symbol "}"
>                                    return (While e r)

-----------------------------------------------------------------------------------------------------

ELSE IF STATEMENTS

> evalElseIf                    :: String -> Prog 
> evalElseIf xs                 = case (parse elseIfParse xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> elseIfParse                   :: Parser Prog 
> elseIfParse                   = do string "if"
>                                    symbol "(" 
>                                    e <- compExpr
>                                    symbol ")"    
>                                    symbol "{"
>                                    p1 <- parseCommands                       --needs to be rreplaced with progParser
>                                    symbol "}"
>                                    ec <- multipleElseIfCase                    -- need to do multiple cases
>                                    string "else"
>                                    symbol "{"
>                                    p2 <- parseCommands                       --needs to be rreplaced with progParser
>                                    symbol "}"
>                                    return (ElseIf e p1 ec p2) 

> multipleElseIfCase            :: Parser [ElseIfCase']
> multipleElseIfCase            = do e <- elseIfCase
>                                    do es <- multipleElseIfCase
>                                       return (e:es)
>                                     +++ return [e]     


> elseIfCase                    :: Parser ElseIfCase'            
> elseIfCase                    = do string "else if"
>                                    symbol "(" 
>                                    e <- compExpr
>                                    symbol ")"    
>                                    symbol "{"
>                                    p1 <- parseCommands
>                                    symbol "}"
>                                    return (Case e p1)   


-----------------------------------------------------------------------------------------------------

IF ESLE STATEMENTS

> evalIfElse                    :: String -> Prog 
> evalIfElse xs                 = case (parse ifElseParse xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> ifElseParse                   :: Parser Prog 
> ifElseParse                   = do string "if" 
>                                    symbol "("
>                                    e <- compExpr    
>                                    symbol ")"
>                                    symbol "{"
>                                    p1 <- parseCommands                       
>                                    symbol "}"
>                                    string "else"                           
>                                    symbol "{"
>                                    p2 <- parseCommands
>                                    symbol "}"
>                                    return (IfElse e p1 p2) 


-----------------------------------------------------------------------------------------------------

IF STATEMENTS 

> evalIf                        :: String -> Prog 
> evalIf xs                     = case (parse ifParse xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> ifParse                       :: Parser Prog 
> ifParse                       = do string "if" 
>                                    symbol "("   
>                                    e <- compExpr
>                                    symbol ")"    
>                                    symbol "{"
>                                    p <- parseCommands
>                                    symbol "}"
>                                    return (If e p)                                   
    
-----------------------------------------------------------------------------------------------------

RETURN 

> evalReturn                    :: String -> Prog 
> evalReturn xs                 =  case (parse return' xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

User return' so as not to get mixed up with haskells "return" function

> return'                       :: Parser Prog
> return'                       =  do string "return"
>                                     e <- compExpr
>                                     return (Return (Just e))


-----------------------------------------------------------------------------------------------------

ASSIGNMENT

> evalAssignment                :: String -> [Prog] 
> evalAssignment xs             =  case (parse multipleAssign xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> assign                        :: Parser Prog
> assign                        =  do n <- getName
>                                     do symbol "="               
>                                        e <- arthExpr
>                                        return (Assign n e)
>                                      +++ do symbol "++"  
>                                             return (Assign n (ExprApp ADD (Var n) (Val (Integer 1))))
>                                      +++ do symbol "--" 
>                                             return (Assign n (ExprApp SUB (Var n) (Val (Integer 1))))
>                                      +++ do symbol "+=" 
>                                             e1 <- arthExpr
>                                             return (Assign n (ExprApp ADD (Var n) e1))
>                                      +++ do symbol "-="
>                                             e2 <- arthExpr
>                                             return (Assign n (ExprApp SUB (Var n) e2))
>                                      +++ do symbol "="
>                                             c <- compExpr
>                                             return (Assign n c)           


Deals with variable name

> getName                       :: Parser Name 
> getName                       = do n1 <- identifier
>                                    return n1

> getName2                      :: Parser String
> getName2                      = do n <- identifier
>                                    string "int"   
>                                    return n      

A Multiple Assignments, deals with a list of MultipleAssignments wich would be global declarations at the top of a 

> multipleAssign                :: Parser [Prog]
> multipleAssign                = do string "global"
>                                    n <- identifier
>                                    symbol "="
>                                    e <- compExpr
>                                    do symbol ";"
>                                       ma <- multipleAssign                                    
>                                       return ((Assign n e):ma)
>                                     +++ do symbol";"
>                                            return [(Assign n e)]  
>                                  +++ do string ""
>                                         return []    

   
-----------------------------------------------------------------------------------------------------

SHOW 

This is a simple print statement that jsut prints out an expression

> evalShow                      :: String -> Prog
> evalShow xs                   =  case (parse parsePrint xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> parseShow                     :: Parser Prog
> parseShow                     = do string "Show"
>                                    symbol "("    
>                                    e <- parseExpr
>                                    symbol ")"
>                                    return (Show e)


> parsePrint                    :: Parser Prog
> parsePrint                    = do string "Print"
>                                    symbol "("
>                                    p <- parseString
>                                    symbol ")"
>                                    return (Print p)


> parseString                   :: Parser String 
> parseString                   = do symbol "\""
>                                    s <- identifier
>                                    symbol "\""
>                                    return s     



-----------------------------------------------------------------------------------------------------

CHANNELS 

Handles the creating of channels and pushing values to a channel

> evalChannels                  :: String -> Prog
> evalChannels xs               =   case (parse parseChanelPush xs) of
>                                       [(e,[])]  -> e
>                                       [(_,out)] -> error ("unused input " ++ out)
>                                       []        -> error "invalid input"
>

> parseChanelCreation           :: Parser Prog
> parseChanelCreation           = do string "var" 
>                                    n <- identifier
>                                    string "chan"
>                                    symbol "="
>                                    string "Make"         
>                                    symbol "("
>                                    string  "chan int"
>                                    symbol ")"
>                                    return (CreateChan n)

> parseChanelPush               :: Parser Prog 
> parseChanelPush               = do c <-  identifier
>                                    symbol "<-"   
>                                    a <- arthExpr
>                                    return (PushToChan c a)   

-----------------------------------------------------------------------------------------------------

CONCURRENT COMMANDS

> evalGoCommand                 :: String -> Prog
> evalGoCommand xs              =   case (parse parseProcessCommand xs) of
>                                       [(e,[])]  -> e
>                                       [(_,out)] -> error ("unused input " ++ out)
>                                       []        -> error "invalid input"

> parseGoCall                   :: Parser Prog
> parseGoCall                   = do string "go "
>                                    i <- identifier
>                                    symbol "("
>                                    do symbol ")"
>                                       return (GoCall i []) 
>                                     +++ do es <- manyExpr
>                                            symbol ")"
>                                            return (GoCall i es)         


> parseProcessCommand           :: Parser Prog
> parseProcessCommand           = do string "Wait()"
>                                    return (Wait)
>                                  +++ do string "Kill()"
>                                         return (Kill) 

-----------------------------------------------------------------------------------------------------

VOID FUNCTION CALLS

> evalVoidFunc                  :: String -> Prog 
> evalVoidFunc xs               =  case (parse parseVoidFun xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> parseVoidFun                  :: Parser Prog
> parseVoidFun                  = do n <- identifier
>                                    symbol "("
>                                    do es <- manyExpr
>                                       symbol ")"    
>                                       return (VoidFuncCall n es)
>                                     +++ do symbol ")"
>                                            return (VoidFuncCall n [])      


-----------------------------------------------------------------------------------------------------

EXPRESSIONS 

> evalExpr                      :: String -> [Expr]
> evalExpr xs                   =  case (parse manyExpr xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> parseExpr                     :: Parser Expr 
> parseExpr                     = do a <- arthExpr
>                                    return a 
>                                  +++ do c <- compExpr
>                                         return c   
 
Handles a list of arguments seperated by a comma returns a list of Expr 
 
TODO: make so can handle an empty list!
 
> manyExpr                      :: Parser [Expr]  
> manyExpr                      = do p1 <- parseExpr 
>                                    do symbol ","
>                                       p2 <- manyExpr
>                                       return (p1:p2)
>                                     +++ return [p1]  

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
>
> subExpr                       :: Parser Expr
> subExpr                       =  do t <- term
>                                     do symbol "-"               
>                                        e <- subExpr
>                                        return (ExprApp SUB t e)
>                                      +++ return t
>

Deals with Division and Multiply currently prioritses division, like Go does!

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
>                                   +++ do string "True"
>                                          return (Val (Integer 1))
>                                   +++ do string "False"
>                                          return (Val (Integer 0))
>                                   +++ do n <- natural
>                                          return (Val (Integer n))
>                                   +++ do i <- identifier
>                                          symbol "("
>                                          ar <- manyExpr
>                                          symbol ")"
>                                          return (FuncCall i ar) 
>                                   +++ do i <- identifier
>                                          symbol "("
>                                          symbol ")"
>                                          return (FuncCall i []) 
>                                   +++ do i <- identifier
>                                          return (Var i)
>                                   +++ do symbol "<-" 
>                                          c <- identifier
>                                          return (PopFromChan c) 
                                                 
 
-----------------------------------------------------------------------------------------------------

COMPARISON EXPRESSIONS

TODO: Fix this to allow only one comparison (don't allow nested comparisons!)

> evalCompExpr                  :: String -> Expr
> evalCompExpr xs               =  case (parse compExpr xs) of
>                                     [(e,[])]  -> e
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

> compExpr                     :: Parser Expr
> compExpr                     =  do n <- neqExpr
>                                    do symbol "=="               
>                                       e <- arthExpr
>                                       return (CompApp EQU n e)
>                                     +++ return n 

> neqExpr                       :: Parser Expr
> neqExpr                       =  do e1 <- grtExpr 
>                                     do symbol "!="               
>                                        e2 <- arthExpr
>                                        return (CompApp NEQ e1 e2)
>                                      +++ return e1

> grtExpr                       :: Parser Expr
> grtExpr                       =  do e1 <- letExpr 
>                                     do symbol ">"               
>                                        e2 <- arthExpr
>                                        return (CompApp GRT e1 e2)
>                                      +++ return e1
>                                    
> letExpr                       :: Parser Expr
> letExpr                       =  do e1 <- geqExpr 
>                                     do symbol "<"               
>                                        e2 <- arthExpr
>                                        return (CompApp LET e1 e2)
>                                      +++ return e1
>
> geqExpr                       :: Parser Expr
> geqExpr                       =  do e1 <- leqExpr 
>                                     do symbol ">="               
>                                        e2 <- arthExpr
>                                        return (CompApp GRT e1 e2)
>                                      +++ return e1

> leqExpr                       :: Parser Expr
> leqExpr                       =  do e1 <- arthExpr 
>                                     do symbol "<="               
>                                        e2 <- arthExpr
>                                        return (CompApp LEQ e1 e2)
>                                      +++ return e1
                                                                                                                      

-----------------------------------------------------------------------------------------------------
                                                                                                     
HELPER FUNCTIONS 
                                                                                                     
> isInteger s                   =   case reads s :: [(Integer, String)] of
>                                       [(_, "")] -> True
>                                       _         -> False
 
> isDouble s                    =   case reads s :: [(Double, String)] of
>                                       [(_, "")] -> True
>                                       _         -> False
 
> isNumeric                     :: String -> Bool
> isNumeric s                   = isInteger s || isDouble s

