
> import Text.ParserCombinators.Parsec
> import System.IO
> import Control.Monad
> import Data.List.Split hiding (endBy, sepBy)

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
> testParse					= parseGo " var a int = 1\n var b int = 3 \n var c int = a + b \n}"

----------------------------------------------------------------

CONVERT THE LIST TOO OP CODE

> convertToOp				:: Either ParseError [[String]] -> Prog 
> convertToOp (Left m)		= Seqn []
> convertToOp (Right ls)	= Seqn [interpretVar cl | cl <- cls]
>								where
>									cls = concat [cleanLinesUp l | l <- ls]
>									
>								
> interpretVar				:: String -> Prog
> interpretVar xs			= Assign (ls !! 1) (Val (asInt (last ls)))
>								where
>									ls = splitOn " " xs
>
>
> cleanLinesUp				:: [String] -> [String]			-- removes white space
> cleanLinesUp xs			= [clean n | n <- xs, n /= "" ]
>								where 
>									clean x = removeSpaceHT(reverse (removeSpaceHT (reverse x)))
>
> removeSpaceHT 			:: String -> String
> removeSpaceHT (x:xs) 		= if (x == ' ') then (removeSpaceHT xs) else x:xs 
>



----------------------------------------------------------------


----------------------------------------------------------------

HELPER FUNCTIONS 

getString as an Int

> asInt 				:: String -> Int
> asInt s 				= (read s :: Int)


----------------------------------------------------------------


