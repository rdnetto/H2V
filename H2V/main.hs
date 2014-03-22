import Language.Haskell.Syntax
import Language.Haskell.Parser as Parser

import AST_Display

--entry point
main = do
    src <- readFile "test.hs"
    putStrLn . displayAST . Parser.parseModule $ src
