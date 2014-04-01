--This module contains code for converting ASTs to DFDs, and for displaying them in the Graphviz format
module DFD (astToDfd, dfdToGraphviz) where

import Control.Monad.State
import Data.List
import Data.Maybe
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

--Used to represent created Graphviz nodes, and the current namespace (implicitly).
type GNode = (HsQName, String)

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

    selectMatch [HsMatch _ name pats (HsUnGuardedRhs exp) decls] = HsLet newDecls exp where      --base case: only one match
        argIDs = map (\x -> HsIdent $ "arg_" ++ show x) [0..]
        args = map fromJust $ filter isJust $ map patternDecls $ zip argIDs pats
        newDecls = (args ++ decls)

    selectMatch [m] = error $ "Failed to match:" ++ pshow m

    --TODO: need to handle pattern matching & destructuring
    selectMatch (m0:ms) = HsIf cond (selectMatch [m0]) (selectMatch ms) where     --perform pattern matching via IFs
        --this technically makes variables accessible where they shouldn't be, but if they are used they'll get shadowed anyway
        HsMatch _ _ pats _ _ = m0       --there is one HsPat for each argument
        --AND together conditions which are not always true
        argIDs = map (\x -> "arg_" ++ show x) [0..]
        cond = combineConds $ map fromJust $ filter isJust $ map patternMatches $ zip argIDs pats

    --returns an expression which is True iff the pattern matches
    --takes an (argument name, pattern) tuple
    --returns Nothing if the pattern will always match
    patternMatches :: (String, HsPat) -> Maybe HsExp
    patternMatches (_, HsPVar _) = Nothing
    patternMatches (_, HsPWildCard) = Nothing
    patternMatches (argId, HsPLit lit@(HsInt val)) = Just $ cleanExpr $ HsInfixApp lhs op rhs where
        lhs = HsVar $ UnQual $ HsIdent argId
        op = HsQVarOp $ UnQual $ HsSymbol "=="
        rhs = HsLit lit
    patternMatches (arg, pat) = error $ printf "%s: unknown pattern%s" arg (pshow pat)

    combineConds :: [HsExp] -> HsExp
    combineConds [x] = x

    --extracts bindings from pattern matching
    patternDecls :: (HsName, HsPat) -> Maybe HsDecl
    patternDecls (argID, var@(HsPVar _)) = Just $ HsPatBind (SrcLoc "" 0 0) var (HsUnGuardedRhs $ HsVar $ UnQual argID) []
    patternDecls (_, HsPLit _) = Nothing
    patternDecls (_, HsPWildCard) = Nothing
    patternDecls (arg, pat) = error $ printf "%s: unknown pattern%s" (show arg) (pshow pat)

processFunc d = error $ "Unknown declaration: " ++ pshow d

--rewrites expressions to remove irrelevant syntactic differences. e.g. infix vs prefix application
--the following white-listed expressions are the only ones we will support
cleanExpr :: HsExp -> HsExp
cleanExpr exp@(HsVar _) = exp
cleanExpr exp@(HsLit _) = exp
cleanExpr (HsLet decls exp) = HsLet (map cleanDecl decls) $ cleanExpr exp
cleanExpr (HsApp e1 e2) = HsApp (cleanExpr e1) (cleanExpr e2)
cleanExpr (HsIf e1 e2 e3) = HsIf (cleanExpr e1) (cleanExpr e2) (cleanExpr e3)
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

--cleans declarations
--TODO: implement pattern matching and destructuring logic here
cleanDecl :: HsDecl -> HsDecl
cleanDecl (HsPatBind src pat rhs decls) = HsPatBind src pat (cleanRHS rhs) (map cleanDecl decls)
cleanDecl (HsFunBind matches) = HsFunBind (map cleanMatch matches)
cleanDecl d = error $ "Unknown declaration: " ++ pshow d

--cleans RHSs
--TODO: this should only handle unguarded RHSs. Guarded RHSs should be refactored at the pattern level
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
--we store an ID counter in the state monad and pass/return the graphviz code between functions
--all monadic functions have a return type of (State stateType returnType). They do not need to take a monadic argument.
newId :: State Int Int
newId =
   do oldID <- get
      let newID = oldID + 1
      put newID
      return oldID

--converts the DFD to a visual representation
dfdToGraphviz :: DFD -> String
dfdToGraphviz (DFD _ allFuncs) = "digraph G{\n" ++ gv ++ "}\n" where
    funcs = foldM renderFunc "" allFuncs
    gv = evalState funcs 0

renderFunc :: String -> Function -> State Int String
renderFunc gv (Function name args expr) = do
    let defnFmt = "func_%s [label=\"%s(%s)\", shape=Mdiamond];\n"
    let defn = printf defnFmt (prettyPrint name) (prettyPrint name) (joinMap ", " prettyPrint args)

    (args, argDecs) <- renderArgs args ("", [])
    let decls = argDecs                                 --TODO: add the ability for one function to call another

    (rootId, expGv) <- renderExpr expr decls
    let rootLink = printf "%s -> func_%s;\n" rootId (prettyPrint name)
    return $ gv ++ defn ++ rootLink ++ args ++ expGv


concatDecls (gv0, decls) (gv, decl) = (gv0 ++ gv, decl:decls)

--args: name
--returns (graphviz, decl), where decl is a 2-tuple of (HsName, nodeID)
renderArg :: HsName -> State Int (String, GNode)
renderArg name = do
    rootId <- liftM id2node newId
    let label = printf "%s [label = \"%s\"];\n" rootId (prettyPrint name)
    return (label, (UnQual name, rootId))

--args: args
--returns: (graphviz, decls)
renderArgs :: [HsName] -> (String, [GNode]) -> State Int (String, [GNode])
renderArgs (arg:args) (graphviz, decls) = do
    (gv, decl) <- renderArg arg
    renderArgs args (gv ++ graphviz, decl:decls)
renderArgs [] x = return x

--declaration resolution:
--let expressions generate the appropriate nodes for their declaractions and populate an association list passed to subexpressions which maps variable names to nodes
--returns: (rootID, graphviz). rootID is the ID of the node rendered. graphviz is the graphviz code generated for it.
renderExpr :: HsExp -> [GNode] -> State Int (String, String)

renderExpr (HsLit literal) _ = do
    rootId <- liftM id2node newId
    let label = printf "%s [label=\"Literal: %s\"];\n" rootId (prettyPrint literal)
    return (rootId, label)

renderExpr (HsVar name) decls = do
    let res = lookup name decls
    if isJust res then
        return (fromJust res, "")

    else
        if (prettyPrint name) `elem` ["(+)", "(-)", "(*)", "(/)"] then do
            --render builtins (temporary hack to workaround our lack of global functions)
            rootId <- liftM id2node newId
            let gv = printf "%s [label=\"Function Call: %s\"];\n" rootId (prettyPrint name)
            return $ (rootId, gv)
        else
            error $ printf "Undefined variable: %s\nDefined tokens: %s" (show name) (show decls)

renderExpr (HsLet newDecls exp) decls = do
    (declGv, argDecs) <- renderDecls decls newDecls ("", [])
    (childId, childGv) <- renderExpr exp (argDecs ++ decls)
    return (childId, declGv ++ childGv)

renderExpr (HsApp f x) decls = do
    rootId <- liftM id2node newId

    (fId, fGv) <- renderExpr f decls
    let fEdge = printf "%s -> %s [color = \"blue\"];\n" fId rootId

    (xId, xGv) <- renderExpr x decls
    let xEdge = printf "%s -> %s;\n" xId rootId

    let label = printf "%s [label=\"->\"];\n" rootId
    return (rootId, label ++ fGv ++ xGv ++ fEdge ++ xEdge)

renderExpr (HsIf cond trueExp falseExp) decls = do
    rootId <- liftM id2node newId

    (cId, cGv) <- renderExpr cond decls
    let cEdge = printf "%s -> %s;\n" cId rootId

    (tId, tGv) <- renderExpr trueExp decls
    let tEdge = printf "%s -> %s [color = \"green\"];\n" tId rootId

    (fId, fGv) <- renderExpr falseExp decls
    let fEdge = printf "%s -> %s [color = \"red\"];\n" fId rootId

    let label = printf "%s [label=\"if black then green else red\"];\n" rootId
    return (rootId, label ++ cGv ++ tGv ++ fGv ++ cEdge ++ tEdge ++ fEdge)

renderExpr exp _ = error $ printf "Unknown expression: " ++ pshow exp

--helper functions
id2node :: Int -> String
id2node id = "node_" ++ (show id)

--TODO: this should be responsible for calling renderFunc
--TODO: need to add guard support...
--args: namespace decl
--returns (graphviz, node)
renderDecl :: [GNode] -> HsDecl -> State Int (String, GNode)
renderDecl ns (HsPatBind _ (HsPVar name) (HsUnGuardedRhs expr) []) = do
    --simplest case: $name = $expr
    (node, gv) <- renderExpr expr ns

    rootId <- liftM id2node newId
    let label = printf "%s [label=\"%s\"];\n" rootId (prettyPrint name)
    let rootEdge = printf "%s -> %s;\n" node rootId
    return (gv ++ label ++ rootEdge, (UnQual name, rootId))

renderDecl ns (HsPatBind src pat rhs subdecls) = do
    --need to render subdeclarations before we can deal with the expression
    (gv0, nodes) <- renderDecls ns subdecls ("", [])
    (gv,  node) <- renderDecl (nodes ++ ns) (HsPatBind src pat rhs [])
    return (gv0 ++ gv, node)

renderDecl _ d = error $ printf "Unknown declaration: " ++ pshow d

--args: namespace decls
--returns: (graphviz, nodes)
renderDecls :: [GNode] -> [HsDecl] -> (String, [GNode]) -> State Int (String, [GNode])
renderDecls ns (decl:decls) (graphviz, nodes) = do
    (gv, node) <- renderDecl ns decl
    renderDecls ns decls (gv ++ graphviz, node:nodes)

renderDecls _ [] x = return x

