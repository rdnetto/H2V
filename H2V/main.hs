import Control.Monad (liftM)
import Data.List (intersect)
import Data.Maybe
import Language.Haskell.Parser as Parser
import System.Environment (getArgs)
import System.Exit
import System.FilePath (replaceExtension)
import Text.Printf (printf)

import AST_Display
import GenerateDFD
import RenderGraphviz
import RenderVerilog

{-
 - We use the following workflow:
 - INPUT -> astToDfd -> dfdToGraphviz -> OUTPUT
 -                   -> dfdToVerilog  -> OUTPUT
 - GenerateDFD:    astToDfd 'cleans' the AST to a simpler form which also has fields to store additional info
 - RenderGraphviz: dfdToGraphviz 'renders' the DFD to Graphviz's textual format.
 - RenderVerilog:  dfdToVerilog 'renders' the DFD to Verilog
 -}

--entry point
main :: IO ()
main = do
    putStrLn "H2V 1.0\nWritten by Reuben D'Netto 2014\n"
    args <- parseArgs =<< getArgs

    --Display help
    if ShowHelp `elem` args
        then displayHelp >> exitWith ExitSuccess
        else return ()

    --Read file
    let hsFile = argFile . head . filter isFile $ args
    src <- readFile hsFile

    --Parse AST
    let ast = case Parser.parseModule src of
            Parser.ParseFailed loc msg -> error $ printf "Parse error at %s: %s" (show loc) msg
            Parser.ParseOk hMod -> hMod

    if OutputAST `elem` args
        then putStrLn . displayAST $ ast
        else return ()

    --Generate DFD
    let dfd = astToDfd ast
    let gvFile = replaceExtension hsFile ".gv"

    if OutputGV `elem` args
        then putStrLn "Generating Graphiviz..." >> writeFile gvFile (dfdToGraphviz dfd)
        else return ()

    --Generate Verilog
    let vFile = replaceExtension hsFile ".v"

    if OutputVerilog `elem` args
        then putStrLn "Generating Verilog..." >> writeFile vFile (dfdToVerilog dfd)
        else return ()

displayHelp :: IO ()
displayHelp = putStrLn $ unlines [
        "Usage: H2V [OPTION] FILE",
        "    -h | --help        Display usage information.",
        "    -a | --ast         Display AST for file. Printed to stdout.",
        "    -g | --graphviz    Convert DFD to Graphviz file. Saved as FILE.gv.",
        "    -v | --verilog     Convert file to Verilog. Saved as FILE.v.",
        ""
    ]

--Code for parsing command-line arguments

data Args = ShowHelp | OutputAST | OutputGV | OutputVerilog | ProcessFile String deriving Eq
argDefs = [ ("-h", ShowHelp),
            ("--help", ShowHelp),
            ("-a", OutputAST),
            ("--ast", OutputAST),
            ("-g", OutputGV),
            ("--graphviz", OutputGV),
            ("-v", OutputVerilog),
            ("--Verilog", OutputVerilog)
          ]

isFile :: Args -> Bool
isFile (ProcessFile _) = True
isFile _ = False

argFile (ProcessFile x) = x

--Returns a list of Args if the provided arguments are valid.
parseArgs :: [String] -> IO [Args]
parseArgs args = if any isNothing res || (length . filter isFile $ catMaybes res) /= 1 && (not . elem ShowHelp $ catMaybes res)
                 then badInput
                 else return . forceOutput $ catMaybes res where

    res = map f args
    badInput = putStrLn "Invalid arguments.\n" >> displayHelp >> exitWith (ExitFailure 1)

    --make sure each arg is valid or a filename
    f a
        | isJust res    = res
        | head a /= '-' = Just $ ProcessFile a
        | otherwise     = Nothing
        where
            res = lookup a argDefs

    --make sure we're actually doing something
    forceOutput args =  if null $ intersect [OutputAST, OutputGV, OutputVerilog] args
                        then OutputVerilog:args
                        else args

