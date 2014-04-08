--This module contains code for rendering DFDs to Graphviz
module RenderGraphviz (dfdToGraphviz) where

import Control.Monad.State
import Data.List
import Data.Maybe
import Text.Printf
import Language.Haskell.Pretty (Pretty, prettyPrint)
import Language.Haskell.Syntax

import Common


--Used to represent created Graphviz nodes, and the current namespace (implicitly).
type GNode = (HsQName, String)


--used to implement a monad for tracking node IDs and graph output in the render graph
--we store an ID counter in the state monad and pass/return the graphviz code between functions
--all monadic functions have a return type of (State stateType returnType). They do not need to take a monadic argument.
newId :: State Int Int
newId =
   do oldID <- get
      let newID = oldID + 1
      put newID
      return oldID

--converts the DFD_old to a visual representation
dfdToGraphviz :: DFD_old -> String
dfdToGraphviz (DFD_old _ allFuncs) = "digraph G{\n" ++ gv ++ "}\n" where
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

