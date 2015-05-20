
Ackermanns function

> ack         :: Int -> Int -> Int
> ack 0 n     = n + 1
> ack m 0     = if (m > 0) then ack (m-1) 1 else 0
> ack m n     = if (m > 0) && (n > 0) then ack (m-1) (ack m (n-1)) else 0