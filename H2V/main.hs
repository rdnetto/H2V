import Data.List
import Text.Printf
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Language.Haskell.Parser as Parser


-- Concatenates the map of a list of elements together, with a delimeter inserted between them.
joinMap delim f list = intercalate delim $ map f list

--applies function f to x N times
iterateN n f x = (iterate f x) !! n

--entry point
main = do
    src <- readFile "test.hs"
    putStrLn . displayAST . Parser.parseModule $ src

--pretty-prints the parsed AST
displayAST (Parser.ParseFailed loc msg) = error $ printf "Parse error at %s: %s" (show loc) msg

--HsModule(srcLoc, mod, exportSpec, importDec, decl)
----srcLoc defines the filename, which we don't care about
----exportSpec defines the functions being exported (we just assume all functions are exported for now)
----importDec imports modules (currently unsupported)
----decl is a list of declarations
displayAST (Parser.ParseOk (HsModule _ mod _ _ decls)) = printf "Module: %s\n%s" moduleName declarations where
    Module moduleName = mod
    declarations = displayDecls 0 decls

    --displays a series of declarations, with the appropriate indentation
    displayDecls :: Int -> [HsDecl] -> String
    displayDecls indentation decls = unlines . indent . lines $ raw where
        raw = joinMap "\n" (displayDecl indentation) decls                          --line separated declarations, unindented

        indent :: [String] -> [String]
        indent = iterateN (indentation + 1) (map $ \ x -> "\t" ++ x)                --function for indenting lines

    --displays a single declaration
    displayDecl :: Int -> HsDecl -> String

    ----function
    displayDecl indentation (HsFunBind matches@(m0:_)) = printf "Function: %s\n\t\t" funcName ++ joinMap "\n\t\t" displayMatch matches where
        HsMatch _ name _ _ _ = m0
        funcName = case name of                     --name is a HsIdent
            HsIdent  n -> n
            HsSymbol n -> "'" ++ n ++ "'"

        --HsMatch(src, name, pattern, rhs, decl)
        --we discard the name because it will be the same for all clauses of a function
        --pattern is a list of the arguments, as defined for pattern matching (e.g. literals, tuples, etc.)
        --RHS is the expression assigned to it
        --decl is the internal declarations. i.e. where clauses. These need to be included with higher level of declarations in 
        --  identifier resolution
        displayMatch :: HsMatch -> String
        displayMatch (HsMatch _ _ pattern rhs decls) = printf fmt (show pattern) (show rhs) declStr where
            fmt = "Match:\n\t\t\tPattern: %s\n\t\t\tRHS: %s\n\t\t\tDeclarations:\n\t%s"
            declStr = displayDecls (indentation + 2) decls

    ----pattern binding (e.g. where statement)
    displayDecl indentation (HsPatBind _ pattern rhs decls) = printf fmt (show pattern) (show rhs) declStr where
        fmt = "Pattern Binding:\n\t\tPattern: %s\n\t\tRHS: %s\n\t\tDeclarations:\n\t%s"
        declStr = displayDecls (indentation + 2) decls


    ----everything else
    displayDecl _ unknown = printf "Unknown declaration:\n\t%s\n\t%s" (show unknown) (prettyPrint unknown)