
> import Text.ParserCombinators.Parsec
> import System.IO
> import Control.Monad
> import Data.List.Split hiding (endBy, sepBy)
> import Data.List

----------------------------------------------------------------

Same Data Structures as used by the compiler

Imperative language:

> data Prog  		=  Assign Name Expr
> 	   		|  If Expr Prog Prog
> 	   		|  While Expr Prog
> 	   		|  Seqn [Prog]
>		           deriving Show
>
> data Expr  		=  Val Int | Var Name | App Op Expr Expr
>			   deriving Show
>
> type Name  		=  String
>
> data Op    		=  Add | Sub | Mul | Div
>			   deriving Show
>
>
>
> data SyntaxError = SynError String
>						deriving Show
>


----------------------------------------------------------------

----------------------------------------------------------------

SIMPLE CSV EXAMPLE USING PARSEC


> csvFile 					= endBy csvLine csvEol
> csvLine 					= sepBy csvCell (char ',')
> csvCell 					= many (noneOf ",\n")
> csvEol 					= char '\n'
>
> parseCSV 					:: String -> Either ParseError [[String]]
> parseCSV input 			= parse csvFile "(unknown)" input

----------------------------------------------------------------

----------------------------------------------------------------

VERY SIMPLE GOLANG LINE EXTRACTER

Gets each line of the file and ends with }

> goFile 					= endBy goLine goEol
> goLine 					= sepBy (many (noneOf "\n}")) (char '\n') 
> goEol 					= char '}'
>
> parseGo 					:: String -> Either ParseError [[String]]
> parseGo input 			= parse goFile "(unknown)" input


----------------------------------------------------------------

> testParse					:: Either ParseError [[String]]
> testParse					= parseGo " var a int = 1\n var b int = 3 \n var c int = a+b \n}"

----------------------------------------------------------------

CONVERT THE LIST TOO OP CODE

> convertToOp				:: Either ParseError [[String]] -> Prog 
> convertToOp (Left m)		= Seqn []
> convertToOp (Right ls)	= Seqn [dealWithAss cl | cl <- cls]
>								where
>									cls = concat [cleanLinesUp l | l <- ls]
>
>
> dealWithAss				:: String -> Prog
> dealWithAss xs			= Assign (ns !! 1) (interpretExp (clean (ls)))
>								where
>									ls = cleanLinesUp (splitOn "=" xs)
>									ns = splitOn " " (head ls)
>									
>
>
> interpretExp				:: String -> Expr
> interpretExp xs 			
>							| (isInfixOf "+" xs)		= interpretApp xs   									
>							| otherwise				= Val (asInt xs) 	
>
>
> interpretApp 				:: String -> Expr								-- Just for Add right now
> interpretApp	xs			= App Add (ls !! 1) (ls !! 0)  
>								where 
>									ls = cleanLinesUp (splitOn "+" xs)
>




----------------------------------------------------------------


----------------------------------------------------------------

HELPER FUNCTIONS 

getString as an Int

> asInt 					:: String -> Int
> asInt s 					= (read s :: Int)

> removeSpaceH	 			:: String -> String
> removeSpaceH (x:xs) 		= if (x == ' ') then (removeSpaceH xs) else x:xs 

> cleanLinesUp				:: [String] -> [String]			-- removes white space
> cleanLinesUp xs			= [clean n | n <- xs, n /= "" ]
>
>
> clean 					:: String -> String 
> clean x 					= removeSpaceH(reverse (removeSpaceH (reverse x)))
>


----------------------------------------------------------------


