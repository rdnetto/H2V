--This module contains code for rendering DFDs to Graphviz
module RenderGraphviz (dfdToGraphviz) where

import Data.List
import Text.Printf

import Common
import DfdDef

--NOTE: We currently get duplicate edges for closures. This will be resolved by implementing closure rewriting.

--If there is an (undirected) loop in the graph, all nodes above it are defined and assigned multiple times.
--Therefore, we tag each node definition with a UID, to eliminate duplicate definitions.
--This functional approach is cleaner, but less efficient than the monadic/stateful approach.
data GNodeDef = GNodeDef {gnodeId :: NodeId,
                          nodeDefs :: String,
                          edgeDefs :: String}

dfdToGraphviz :: DProgram -> String
dfdToGraphviz dfds = printf "digraph G{\n%s}\n" (concat ns ++ concat es) where
    (ns, es) = splitTuple $ map renderFunc dfds

--node definitions and edges are returned separately, to ensure nodes are placed in the correct subgraph
renderFunc :: DFD -> (String, String)
renderFunc dfd@(DFD dfdID name args _ _ root) = (nDefs, edges) where
    (subNs, subEs) = extractGnode $ renderNode root

    nDefs = unlines [ printf "subgraph cluster_dfd_%i{" dfdID,
                    printf "label = \"%s [dfd_%i] (%i args)\";" name dfdID $ length args,
                    "color = black;",
                    concatMap renderArg . zip [0..] $ trueArgs dfd,
                    subNs,
                    printf "result_%i [ label = \"Result\", color=red ];" dfdID,
                    "}\n"
                ]
    edges = unlines [
                    subEs,
                    printf "node_%i -> result_%i;" (nodeID root) dfdID
            ]

--Args are defined by the function, so we don't need to worry about duplicates
renderArg :: (Int, DNode) -> String
renderArg (i, DArgument argID t par)
    | isList t  = printf "node_%i [ label = \"arg_%i\n%s[node_%i]\", color=red ];\n" argID i (renderPar par) argID
    | otherwise = printf "node_%i [ label = \"arg_%i\n[node_%i]\", color=red ];\n" argID i argID

renderNode :: DNode -> [GNodeDef]
renderNode (DLiteral nodeID value) = return $ GNodeDef nodeID (printf "node_%i [ label = \"%i\n[node_%i]\"];\n" nodeID value nodeID) ""
renderNode (DArgument _ _ _) = []                                     --arguments are defined as part of the function

renderNode (DVariable varID _ val) = (GNodeDef varID node edge):valDef where
    valDef = renderNode val
    node = printf "node_%i [ fontcolor=dimgray ];\n" varID
    edge = printf "node_%i -> node_%i;\n" (nodeID val) varID

renderNode (DListLiteral listID elements par) = (GNodeDef listID node edge):eDefs where
    node = printf "node_%i [ label = \"List\n%s[node_%i]\", color=blue ];\n" listID parLbl listID
    edge = concatMap (subEdge listID "[%i]") $ zip [0..] elements
    eDefs = concatMap renderNode elements
    parLbl = renderPar par

renderNode (DFunctionCall appID f args par) = (GNodeDef appID node edge):aDefs where
    fID = if   (dfdID f) == -1
          then ""
          else printf "(dfd_%i)" $ dfdID f
    node = printf "node_%i [ label = \"Function call: %s %s\n%s[node_%i]\", color=darkgreen ];\n" appID (dfdName f) fID parLbl appID
    edge = concatMap (subEdge appID "arg_%i") $ zip [0..] args
    aDefs = concatMap renderNode args
    parLbl = renderPar par

renderNode (DFunction fID f) = (GNodeDef fID node ""):[] where
    node = printf "node_%i [ label = \"Function: %s\n[node_%i]\", color=darkgreen ];\n" fID (dfdName f) fID

renderNode (DTupleElem elemID tupleIndex tuple) = (GNodeDef elemID node edge):tupleDef where
    desc = ["Head", "Tail", "HeadExists"] !! tupleIndex
    node = printf "node_%i [ label = \"%s\n[node_%i]\", color=magenta ];\n" elemID desc elemID
    edge = printf "node_%i -> node_%i;\n" (nodeID tuple) elemID
    tupleDef = renderNode tuple

--Helper function for generating edges from a node to its children.
--iFmt is a printf format string used for the edge's indexed label
subEdge :: NodeId -> String -> (Int, DNode) -> String
subEdge parentID iFmt (i, a) = printf "node_%i -> node_%i [ label = \"%s\" ];\n" (nodeID a) parentID indexStr where
    indexStr :: String = printf iFmt i

--Helper function for describing parallelism
renderPar :: Parallelism -> String
renderPar NoPar = ""
renderPar par
    | isAssigned par = res ""
    | otherwise      = res " (inferred)"
    where
        res desc = printf "Par: %i%s\n" (parValue par) desc

extractGnode :: [GNodeDef] -> (String, String)
extractGnode ns = (concatMap nodeDefs ns', concatMap edgeDefs ns') where
    ns' = uniqWith f ns
    f a b = gnodeId a == gnodeId b

--Filters out key-value pairs which re-use existing keys. Preserves order.
uniq :: Ord a => [(a, b)] -> [(a, b)]
uniq xs = reverse $ foldl f [] xs where
    f res (key, value)
        | key `elem` (map fst res) = res                                    --key already defined; do nothing
        | otherwise                = (key, value):res                       --key not defined; add definition

--Filters out duplicates elements, using a custom equality test. Preserves order.
uniqWith :: (a -> a -> Bool) -> [a] -> [a]
uniqWith f xs = reverse $ foldl f' [] xs where
    f' res x
        | any (f x) res = res                                               --element in list; do nothing
        | otherwise     = x:res                                             --element not in list; prepend
