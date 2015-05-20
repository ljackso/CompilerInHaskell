
import Parsing
import GoalMachine

--Parsing Example;

data Expr       = CompExpr Op Expr Expr | Val Number                    
data Op         = EQU | NEQ
type Number     = Int

parseOp         :: Parser Op
parseOp         = do symbol "=="
                     return ( EQU )
                   +++ do symbol "!="
                          return ( NEQ )

evalCompExpr    :: String -> Expr
evalCompExpr xs = case ( parse parseComp xs ) of
                    [( e ,[])] -> e
                    [( _ , o )] -> error (" unused "++ o )
                    [] -> error " invalid "
                    
parseComp       :: Parser Expr
parseComp       = do n1 <- natural
                     o <- parseOp
                     n2 <- natural
                     return (CompExpr o (Val n1) (Val n2))
