--This module contains code for rendering DFDs to Verilog
module RenderVerilog (dfdToVerilog) where

import Control.Monad
import Data.List
import Text.Printf

import Common
import DfdDef

--BUG: linking is incomplete; there are still function headers in the DFD. They seem to be for fib' (recursive case) only...

--QUESTION: How do we handle recursive functions in a sane manner?
--We need special logic for them since the tail-calls will (probably) be defined using their headers.
--We need to mark them as different, because they're generated differently
--
--SOLUTION:
--Don't need to mark them if we respond immediately on detection
--Can detect non-mutual case (i.e. a single function) using dmap
--
--IDEA: implement closure support using partial application. This simplifies rewriting application, and issues with nested closures

--If there is an (undirected) loop in the graph, all nodes above it are defined and assigned multiple times.
--Therefore, we tag each node definition with a UID, to eliminate duplicate definitions.
--This functional approach is cleaner, but less efficient than the monadic/stateful approach.
type VNodeDef = (NodeId, String)

dfdToVerilog :: DProgram -> String
dfdToVerilog dfds = concatMap renderFunc dfds

--Returns True if f calls target. A function which calls itself is recursive.
--This is transitive if indirect is True. i.e. if f1 calls f2 and f2 calls f3, then f1 calls f3.
--Otherwise, only the calls directly within f are considered. i.e. self-recursion
--TODO: need to add check for arguments, once first class functions are in use
fCalls :: DFD -> Bool -> DFD -> Bool
fCalls target indirect f = eCalls target indirect $ dfdRoot f

--Returns True if an expression or sub-expression calls target.
eCalls :: DFD -> Bool -> DNode -> Bool
eCalls target indirect e = dfold (\a -> \b -> a || eCalls' b) False e where
    eCalls' :: DNode -> Bool
    eCalls' (DFunctionCall _ fc _) = (dfdID fc == dfdID target) || (indirect && fCalls target indirect fc)
    eCalls' _ = False

--A tail-recursion DFD will be a tree of IFs, where each leaf is a recursive call or a non-recursive expression.
--Returns a list of 2-tuples representing each leaf.
--The first element of the tuple is a list of conditions, where left conditions must be negated.
--    Earlier conditions have higher precedence.
--The second element of the tuple is:
--  Left: a non-recursive expression
--  Right: a list of arguments for the recursive call
--
--Assumes function is self-recursive - use `calls` to check this.
recursiveCases :: DFD -> [([Either DNode DNode], Either DNode [DNode])]
recursiveCases f = recExpr [] $ dfdRoot f where
    recExpr :: [Either DNode DNode] -> DNode -> [([Either DNode DNode], Either DNode [DNode])]
    recExpr conds node
        | isIf node           = (recExpr (trueCond:conds) trueBranch) ++ (recExpr (falseCond:conds) falseBranch)
        | isFunctionCall node = return (conds, Right $ callArgs node)
        | eCalls f False node = error "Non-tail recursion is not supported"
        | otherwise           = return (conds, Left node)                   --is expression
        where
        [cond, trueBranch, falseBranch] = callArgs node
        trueCond = Right cond
        falseCond = Left cond

--Render combinatorial functions.
--TODO: add assign statements to link ready/done signals
renderFunc :: DFD -> String
renderFunc dfd@(DFD resID name args _ _ root)
    | fCalls dfd True dfd  = renderRecursiveFunc dfd $ recursiveCases dfd
    | fCalls dfd False dfd = error "Mutual recursion is not supported"
    | otherwise            = unlines [ printf "module dfd_%i(" resID,
                                       "input clock, input ready, output done,",
                                       printf "//%s (%i args)" name $ length args,
                                       concatMap renderArg $ zip [0..] args,
                                       printf "output [7:0] node_%i" resID,
                                       ");",
                                       "assign done = ready;",
                                       concatMap snd . uniq $ renderNode root,
                                       printf "assign node_%i = node_%i;" resID (nodeID root),
                                       "endmodule\n"
                                     ]

--This function renders a recursive function
--WIP
--structure: input -> comb logic -> registers -> comb logic ...
renderRecursiveFunc :: DFD -> [([Either DNode DNode], Either DNode [DNode])] -> String
renderRecursiveFunc (DFD resID name args _ _ root) recCases = res where
    res = unlines [ --Synchronous logic
                    printf "module dfd_%i(" resID,
                    "input clock, input ready, output done,"
                    printf "//%s (%i args)" name $ length args,
                    concatMap renderArg $ zip [0..] args,
                    printf "output [7:0] node_%i" resID,
                    ");",

                    concatMap snd . uniq $ renderNode root,
                    printf "assign node_%i = node_%i;" resID (nodeID root),

                    "endmodule\n",

                    --Combinatorial logic
                    "module dfd_%i_cmb(" resID,
                    printf "//%s (%i args)" name $ length args,
                    concatMap renderArg $ zip [0..] args,
                    "//Args for recursive call",
                    "output recurse,",                                  --if true, then perform another iteration
                    concatMap renderArg $ zip [0..] args,               --TODO: implement this
                    printf "output [7:0] node_%i" resID,
                    ");",
                    "assign done = ready;",
                    concatMap snd . uniq $ renderNode root,
                    printf "assign node_%i = node_%i;" resID (nodeID root),
                    --TODO: add assign statements for recursive call args
                    "endmodule\n"
                  ]

--Args are defined by the function, so we don't need to worry about duplicates
renderArg :: (Int, (NodeId, DType)) -> String
renderArg (i, (argID, t)) = printf "input %s node_%i, //arg %i\n" (vType t) argID i

--WIP: this function crashes because getting a header for the current function is meaningless;
--we should be using recCases in renderRecursiveFunc() to generate DFDs for the arguments / recursive call
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
