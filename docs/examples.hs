------------------------------------------------------------------------------------------------------------------------
-- Pure combinatorial functions are not worth bothering with, since Verilog's native abilities are sufficient for them

------------------------------------------------------------------------------------------------------------------------
-- Tail-recursive functions

-- Returns the nth fibonnacci number
-- Could be implemented using an infinite list, but that would be more painful to compile to Verilog
fib n = fib' 0 1 n where
    fib' x0 _ 0 = x0
    fib' x0 x1 n = fib' x1 (x0 + x1) (n - 1)

------------------------------------------------------------------------------------------------------------------------
-- List processing functions (need DMA, and possibly higher order functions)

-- sum, map, fold, etc.

-- vector dot product, matrix product
