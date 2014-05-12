--This module contains code for rendering DFDs to Graphviz
module RenderGraphviz (dfdToGraphviz) where

import Data.List
import Text.Printf

import Common
import DfdDef


--If there is an (undirected) loop in the graph, all nodes above it are defined and assigned multiple times.
--Therefore, we tag each node definition with a UID, to eliminate duplicate definitions.
--This functional approach is cleaner, but less efficient than the monadic/stateful approach.
type GNodeDef = (NodeId, String)

dfdToGraphviz :: DProgram -> String
dfdToGraphviz dfds = printf "digraph G{\n%s}\n" $ concatMap renderFunc dfds

renderFunc :: DFD -> String
renderFunc (DFD resID name args _ _ root) = res where
    res = unlines [ printf "subgraph cluster_dfd_%i{" resID,
                    printf "label = \"%s (%i args)\";" name $ length args,
                    "color = black;",
                    concatMap renderArg $ zip [0..] args,
                    concatMap snd . uniq $ renderNode root,
                    printf "node_%i [ label = \"Result\", color=red ];" resID,
                    printf "node_%i -> node_%i;" (nodeID root) resID,
                    "}"
                  ]

--Args are defined by the function, so we don't need to worry about duplicates
renderArg :: (Int, (NodeId, DType)) -> String
renderArg (i, (argID, _)) = printf "node_%i [ label = \"arg_%i\", color=red ];" argID i

renderNode :: DNode -> [GNodeDef]
renderNode (DLiteral nodeID value) = return (nodeID, printf "node_%i [ label = \"%i\"];\n" nodeID value)
renderNode (DVariable _ _ Nothing) = []                                 --arguments are defined as part of the function

renderNode (DVariable varID _ (Just val)) = valDef ++ return (varID, res) where
    valDef = renderNode val
    res =  printf "node_%i [ fontcolor=dimgray ];\n" varID
        ++ printf "node_%i -> node_%i;\n" (nodeID val) varID

renderNode (DFunctionCall appID f args) = aDefs ++ return (appID, res) where
    res =  printf "node_%i [ label = \"Function call: %s\", color=darkgreen ];\n" appID (dfdName f)
        ++ (concatMap argEdge $ zip [0..] args)

    aDefs = concatMap renderNode args

    argEdge :: (Int, DNode) -> String
    argEdge (i, a) = printf "node_%i -> node_%i [ label = \"arg_%i\" ];\n" (nodeID a) appID i

--Filters out key-value pairs which re-use existing keys. Preserves order.
uniq :: Ord a => [(a, b)] -> [(a, b)]
uniq xs = reverse $ foldl f [] xs where
    f res (key, value)
        | key `elem` (map fst res) = res                                    --key already defined; do nothing
        | otherwise                = (key, value):res                       --key not defined; add definition
