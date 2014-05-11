import Text.Printf
import Language.Haskell.Syntax
import Language.Haskell.Parser as Parser

import AST_Display
import GenerateDFD
import RenderGraphviz

{-
 - We use the following workflow:
 - INPUT -> astToDfd -> dfdToGraphviz -> OUTPUT
 -                   -> dfdToVerilog  -> OUTPUT
 - GenerateDFD:    astToDfd 'cleans' the AST to a simpler form which also has fields to store additional info
 - RenderGraphviz: dfdToGraphviz 'renders' the DFD to Graphviz's textual format.
 - RenderVerilog:  dfdToVerilog 'renders' the DFD to Verilog
 -
 - Shared data types are placed in the Utility module to avoid dependency loops
 -}


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

    writeFile "test.gv" $ dfdToGraphviz dfd

