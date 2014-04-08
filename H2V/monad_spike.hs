import Control.Monad.State

type NGen = State (Int, String)

fmap2 :: (a -> a, b -> b) -> (a, b) -> (a, b)
fmap2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

newId :: NGen Int
newId = do
    (x, _) <- get
    modify $ fmap2 (succ, id)
    return x

newSymbol = do
    modify $ fmap2 (id, ('x':))
    return ()

main = do
    --We can't create a list of functions with different return types, which makes testing this harder...
    let m = newId >> newSymbol >> newId           --This is valid
    let x = execState m (0, "")
    putStrLn $ show x
