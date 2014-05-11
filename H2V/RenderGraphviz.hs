--This module contains code for rendering DFDs to Graphviz
module RenderGraphviz (dfdToGraphviz) where

import Text.Printf

import Common
import DfdDef


dfdToGraphviz :: DProgram -> String
dfdToGraphviz dfds = printf "digraph G{\n%s}\n" $ concatMap renderFunc dfds

renderFunc :: DFD -> String
renderFunc (DFD resID name _ _ root) = res where
    res = unlines [ printf "subgraph cluster_dfd_%i{" resID,
                    printf "label = \"%s\";" name,
                    "color = black;",
                    renderNode root,
                    printf "node_%i [ label = \"Result\", color=red ];" resID,
                    printf "node_%i -> node_%i;" (nodeID root) resID,
                    "}"
                  ]

renderNode :: DNode -> String
renderNode (DLiteral nodeID value) = printf "node_%i [ label = \"%i\"];\n" nodeID value
--TODO: Remove this; nodes should be defined when they are set, otherwise duplicate definitions occur.
--      This requires that the function header store the no. of args.
renderNode (DVariable varID _ Nothing) = printf "node_%i [label = \"<ARG>\", color=red ];\n" varID

renderNode (DVariable varID _ (Just val)) = valDef ++ res where
    valDef = renderNode val
    res =  printf "node_%i [ fontcolor=dimgray ];\n" varID
        ++ printf "node_%i -> node_%i;\n" (nodeID val) varID

renderNode (DFunctionCall appID f@(DFD fID fName _ _ _) args) = res where
    res =  printf "node_%i [ label = \"Function call: %s\", color=darkgreen ];\n" appID fName
        ++ (concatMap argEdge $ zip [0..] args)
    argEdge :: (Int, DNode) -> String
    argEdge (i, a) = line1 ++ line2 where
        line1 = renderNode a
        line2 = printf "node_%i -> node_%i [ label = \"arg_%i\" ];\n" (nodeID a) appID i

