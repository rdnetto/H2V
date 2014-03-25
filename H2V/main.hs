import Text.Printf
import Language.Haskell.Syntax
import Language.Haskell.Parser as Parser

import AST_Display
import DFD

--entry point
main :: IO ()
main = do
    src <- readFile "test.hs"

    let ast = case Parser.parseModule src of
            Parser.ParseFailed loc msg -> error $ printf "Parse error at %s: %s" (show loc) msg
            Parser.ParseOk hMod -> hMod

    --display AST
    --putStrLn . displayAST $ ast

    let dfd = astToDfd ast
    putStrLn $ show dfd

    writeFile "test.gv" $ dfdToGraphviz dfd

