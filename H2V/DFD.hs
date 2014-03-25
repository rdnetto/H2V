--This module contains code for converting ASTs to DFDs, and for displaying them in the Graphviz format
module DFD (astToDfd, dfdToGraphviz) where

import Control.Monad.State
import Data.List
import Text.Printf
import Language.Haskell.Pretty (Pretty, prettyPrint)
import Language.Haskell.Syntax

import Utility

--DFD coreFuncs allFunctions
--coreFuncs contains the names of functions which are exported
--allFunctions is a list of all the functions defined in the file root of the file
data DFD = DFD [HsName] [Function] deriving Show

--Function(name, arguments, expression)
--arguments is a list of name-type tuples (names will be generated if none are present)
data Function = Function HsName [HsName] HsExp deriving Show

--helper function used to display parsing errors
pshow :: Show a => Pretty a => a -> String
pshow x = "\n" ++ show x ++ "\n" ++ prettyPrint x

--converts an AST to a DFD
astToDfd :: HsModule -> DFD
astToDfd (HsModule _ hMod exportSpec _ decls) = DFD coreFuncs allFuncs where
    coreFuncs = case exportSpec of
                Just exports -> map exportName $ exports
                Nothing -> [HsIdent "main"]                         --if there is no export spec, assume the function is called main
    allFuncs = map processFunc decls

--generates the DFD object for a function
processFunc :: HsDecl -> Function
processFunc (HsFunBind matches) = Function name args expr where
    name = case matches of                                          --use the name from the first match clause
           (HsMatch _ name _ _ _):_ -> name

    --a list of argument names wrapped in Maybe to handle undefined names
    args' = map firstPVar $ transpose $ map getPatterns matches where
        getPatterns (HsMatch _ _ p _ _) = p                         --get the arg names from a match clause
        firstPVar :: [HsPat] -> Maybe HsName                        --returns the first defined name for an argument
        firstPVar (HsPVar arg:_) = Just arg
        firstPVar (_:xs) = firstPVar xs
        firstPVar [] = Nothing

    --replace the null names with generated ones
    args = map f $ zip [0..] args' where
        f (i, Nothing) = HsIdent ("arg_" ++ show i)
        f (_, Just x) = x

    --add match selection logic to expression tree
    expr = cleanExpr $ selectMatch matches

    --selects the match expression to use, and wrap it in a HsLet to preserve where-declarations
    selectMatch :: [HsMatch] -> HsExp
    selectMatch [HsMatch _ _ pat (HsUnGuardedRhs exp) decls] = HsLet decls exp  --TODO: assume only one match object for now

processFunc d = error $ "Unknown declaration: " ++ pshow d

--rewrites expressions to remove irrelevant syntactic differences. e.g. infix vs prefix application
--the following white-listed expressions are the only ones we will support
cleanExpr :: HsExp -> HsExp
cleanExpr exp@(HsVar _) = exp
cleanExpr exp@(HsLit _) = exp
cleanExpr (HsLet decls exp) = HsLet (map cleanDecl decls) $ cleanExpr exp
cleanExpr (HsApp e1 e2) = HsApp (cleanExpr e1) (cleanExpr e2)
--convert infix application to prefix application
cleanExpr (HsInfixApp arg1 op arg2) = case op of
                                        HsQVarOp opName -> newExpr opName
                                        HsQConOp opName -> newExpr opName
                                      where
    newExpr opName = HsApp (HsApp (HsVar opName) arg1') arg2'
    arg1' = cleanExpr arg1
    arg2' = cleanExpr arg2
--convert the unary negation operator to subtraction. e.g. -x => 0 - x => (-) 0 x
cleanExpr (HsNegApp exp) = cleanExpr $ HsInfixApp (HsLit $ HsInt $ 0) (HsQVarOp $ UnQual $ HsSymbol "-") (cleanExpr exp)
--remove parentheses, since they're redundant
cleanExpr (HsParen exp) = cleanExpr exp

cleanExpr exp = error $ "Unknown expression: " ++ pshow exp

--cleans declarations, by recursing through them to call cleanExpr
cleanDecl :: HsDecl -> HsDecl
cleanDecl (HsPatBind src pat rhs decls) = HsPatBind src pat (cleanRHS rhs) (map cleanDecl decls)
cleanDecl (HsFunBind matches) = HsFunBind (map cleanMatch matches)
cleanDecl d = error $ "Unknown declaration: " ++ pshow d

--cleans RHSs
cleanRHS :: HsRhs -> HsRhs
cleanRHS (HsUnGuardedRhs exp) = HsUnGuardedRhs (cleanExpr exp)
cleanRHS (HsGuardedRhss guards) = HsGuardedRhss (map f guards) where
    f (HsGuardedRhs src e1 e2) = HsGuardedRhs src (cleanExpr e1) (cleanExpr e2)

--cleans function matches
cleanMatch :: HsMatch -> HsMatch
cleanMatch (HsMatch src name pats rhs decls) = HsMatch src name pats (cleanRHS rhs) (map cleanDecl decls)

--converts an export into an unqualified name
exportName :: HsExportSpec -> HsName
exportName (HsEVar name) = unqual name
exportName e = error $ "Unknown exportSpec: " ++ pshow e

--convert a qualified name to an unqualified name
unqual :: HsQName -> HsName
unqual (UnQual n) = n
unqual q = error $ "Unknown HsQName: " ++ pshow q

--used to implement a monad for tracking node IDs and graph output in the render graph
--functions do not expressly take the state as an argument; they simply return a monad of the appropriate type
newId :: State Int Int                  --State stateType returnType
newId =
   do oldID <- get
      let newID = oldID + 1
      put newID
      return oldID

--runID :: State Int a -> a
--runID m = evalState m 0

--converts the DFD to a visual representation
--NOTE: it may actually be easier to generate Verilog than to generate a visual graph, since a visual graph needs unique node IDs
dfdToGraphviz :: DFD -> String
dfdToGraphviz (DFD _ allFuncs) = "digraph G{\n" ++ gv ++ "}" where
    funcs = foldM renderFunc "" allFuncs
    gv = evalState funcs 0


renderFunc :: String -> Function -> State Int String
renderFunc gv (Function name args expr) = do
    id <- newId
    return $ gv ++ "Assigned id " ++ (show id) ++ " to function " ++ (show name) ++ "\n"

{-
    let defn = printf "func_%s [label=\"%s(%s)\", shape=Mdiamond];\n" (prettyPrint name) (prettyPrint name) (joinMap ", " prettyPrint args)
    let exp = renderExpr expr                       --this will need to use newId later

    return gv ++ defn ++ exp
-}
renderExpr _ = "//expr goes here\n"
