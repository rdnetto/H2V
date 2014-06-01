--This module contains code for rendering DFDs to Verilog
module RenderVerilog (dfdToVerilog) where

import Data.List
import Text.Printf

import Common
import DfdDef

--BUG: linking is incomplete; there are still function headers in the DFD. They seem to be for fib' (recursive case) only...

--If there is an (undirected) loop in the graph, all nodes above it are defined and assigned multiple times.
--Therefore, we tag each node definition with a UID, to eliminate duplicate definitions.
--This functional approach is cleaner, but less efficient than the monadic/stateful approach.
type VNodeDef = (NodeId, String)

dfdToVerilog :: DProgram -> String
dfdToVerilog dfds = concatMap renderFunc dfds

renderFunc :: DFD -> String
renderFunc (DFD resID name args _ _ root) = res where
    res = unlines [ printf "module dfd_%i(" resID,
                    printf "//%s (%i args)" name $ length args,
                    concatMap renderArg $ zip [0..] args,
                    printf "output [7:0] node_%i" resID,
                    ");",
                    concatMap snd . uniq $ renderNode root,
                    printf "assign node_%i = node_%i;" resID (nodeID root),
                    "endmodule\n"
                  ]

--Args are defined by the function, so we don't need to worry about duplicates
renderArg :: (Int, (NodeId, DType)) -> String
renderArg (i, (argID, t)) = printf "input %s node_%i, //arg %i\n" (vType t) argID i

renderNode :: DNode -> [VNodeDef]
renderNode (DLiteral nodeID value) = return (nodeID, res) where
    res =  printf "wire %s node_%i;\n" (vType UndefinedType) nodeID
        ++ printf "assign node_%i = %i;\n" nodeID value

renderNode (DVariable _ _ Nothing) = []                                     --arguments are defined as part of the function

renderNode (DVariable varID t (Just val)) = valDef ++ return (varID, res) where
    valDef = renderNode val
    res =  printf "wire %s node_%i;\n" (vType t) varID
        ++ printf "assign node_%i = node_%i;\n" varID (nodeID val)

renderNode (DFunctionCall appID f args)
    | dfdID f == (-1) = aDefs ++ return (renderBuiltin appID (builtinOp $ dfdRoot f) args)
    | otherwise       = aDefs ++ return (appID, res)
    where
        res =  printf "wire %s node_%i;\n" (vType $ returnType f) appID                     --BUG: there's a header here...
            ++ printf "dfd_%i(%s node_%i);\n" (dfdID f) (concatMap argEdge args) appID
        aDefs = concatMap renderNode args

        argEdge :: DNode -> String
        argEdge a = printf "node_%i, " (nodeID a)

renderBuiltin :: NodeId -> BuiltinOp -> [DNode] -> VNodeDef
renderBuiltin resID BitwiseNot (arg:[]) = (resID, res) where
    res =  printf "wire %s node_%i;\n" (vType $ nodeType arg) resID
        ++ printf "assign node_%i = ~node_%i;\n" resID (nodeID arg)

renderBuiltin resID (BinaryOp op) (a0:a1:[]) = (resID, res) where
    res =  printf "wire %s node_%i;\n" (vType $ nodeType a0) resID          --TODO: should really use the larger of the two types
        ++ printf "assign node_%i = node_%i %s node_%i;\n" resID (nodeID a0) op (nodeID a1)

renderBuiltin resID Ternary (cond:tExp:fExp:[]) = (resID, res) where
    res =  printf "wire %s node_%i;\n" (vType $ nodeType tExp) resID        --TODO: should really use the larger of the two types
        ++ printf "assign node_%i = node_%i ? node_%i : node_%i;\n" resID (nodeID cond) (nodeID tExp) (nodeID fExp)

--Converts a Haskell type to a Verilog type (i.e. a bus)
vType :: DType -> String
vType (DUInt n) = printf "[%i:0]" (n - 1)
vType (DSInt n) = printf "[%i:0]" (n - 1)
vType DBool = ""                                --1 bit is implied by a blank type
vType UndefinedType = vType $ DUInt 8           --default

--Filters out key-value pairs which re-use existing keys. Preserves order.
uniq :: Ord a => [(a, b)] -> [(a, b)]
uniq xs = reverse $ foldl f [] xs where
    f res (key, value)
        | key `elem` (map fst res) = res                                    --key already defined; do nothing
        | otherwise                = (key, value):res                       --key not defined; add definition
