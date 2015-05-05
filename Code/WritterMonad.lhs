
> import Control.Monad.Writer 

> isBigGang1            :: Int -> Bool  
> isBigGang1 x          = x > 9

> isBigGang2            :: Int -> (Bool, String)  
> isBigGang2 x          = (x > 9, "Compared gang size to 9.")  

> applyLog1             :: (a,String) -> (a -> (b,String)) -> (b,String)  
> applyLog1 (x,log) f   = let (y,newLog) = f x in (y,log ++ newLog) 

> applyLog2             :: (a,[c]) -> (a -> (b,[c])) -> (b,[c]) 
> applyLog2 (x,log) f   = let (y,newLog) = f x in (y,log ++ newLog)  

> applyLog3             :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
> applyLog3 (x,log) f   = let (y,newLog) = f x in (y,log `mappend` newLog)  

> newtype Writer w a    = Writer { runWriter :: (a,w) }

> tell                  :: Monoid w => w -> Writer w ()

> logNumber x           = writer (x, ["Got number: " ++ show x])  
  
> multWithLog           :: Writer [String] Int  
> multWithLog           = do a <- logNumber 3  
>                            b <- logNumber 5  
>                            return (a*b)      

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


> instance (Monoid w, MonadWriter w m) => MonadWriter w (ST s m)