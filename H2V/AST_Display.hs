module AST_Display (displayAST) where

import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Language.Haskell.Parser as Parser
import Text.Printf

import Common

--pretty-prints the parsed AST
displayAST :: HsModule -> String
displayAST (HsModule _ mod exportSpec _ decls) =
        printf "Module: %s [exports = %s]\n%s" moduleName (show exportSpec) declarations where
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

