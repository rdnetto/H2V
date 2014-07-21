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

--A tail-recursive DFD will be a tree of IFs, where each leaf is a recursive call or a non-recursive expression.
--Returns a list of 2-tuples representing each leaf.
--The first element of the tuple is a list of conditions, where left conditions must be negated.
--    Earlier conditions have higher precedence.
--The second element of the tuple is:
--  Left: a non-recursive expression
--  Right: a list of arguments for the recursive call
type RecursiveCase = ([Either DNode DNode], Either DNode [DNode])

--Conditions for case to be valid. Left nodes need to be negated.
recConds :: RecursiveCase -> [Either DNode DNode]
recConds (x, _) = x

--Returns False if the argument is a base case
isRecursive :: RecursiveCase -> Bool
isRecursive (_, x) = isRight x

--NodeId of root node for base case
baseRoot :: RecursiveCase -> DNode
baseRoot (_, Left x) = x

--Arguments for the recursive call
recArgs :: RecursiveCase -> [DNode]
recArgs (_, Right x) = x

dfdToVerilog :: DProgram -> String
dfdToVerilog dfds = concatMap renderFunc dfds

--Returns True if f calls target. A function which calls itself is recursive.
--This is not transitive. i.e. if f1 calls f2 and f2 calls f3, then f1 does not call f3.
--Otherwise, only the calls directly within f are considered. i.e. self-recursion
--NOTE: need to write a separate function to test for indirect calls, since we recurse infinitely without a stack
--TODO: need to add check for arguments, once first class functions are in use
fCalls :: DFD -> DFD -> Bool
fCalls target f = eCalls target $ dfdRoot f

--Returns True if an expression or sub-expression calls target.
eCalls :: DFD -> DNode -> Bool
eCalls target e = dfold (\a -> \b -> a || eCalls' b) False e where
    eCalls' :: DNode -> Bool
    eCalls' (DFunctionCall _ fc _) = (dfdID fc == dfdID target)
    eCalls' _ = False

--Assumes function is self-recursive - use `calls` to check this.
recursiveCases :: DFD -> [RecursiveCase]
recursiveCases f = recExpr [] $ dfdRoot f where
    recExpr :: [Either DNode DNode] -> DNode -> [([Either DNode DNode], Either DNode [DNode])]
    recExpr conds node
        | isIf node           = (recExpr (trueCond:conds) trueBranch) ++ (recExpr (falseCond:conds) falseBranch)
        | isFunctionCall node = return (conds, Right $ callArgs node)
        | otherwise           = return (conds, Left node)                   --is expression
        where
        [cond, trueBranch, falseBranch] = callArgs node
        trueCond = Right cond
        falseCond = Left cond

--Render combinatorial functions.
--TODO: add assign statements to link ready/done signals
renderFunc :: DFD -> String
renderFunc dfd@(DFD dfdID name args _ _ root)
    | fCalls dfd dfd = renderRecursiveFunc dfd $ recursiveCases dfd
    | otherwise      = unlines [printf "module dfd_%i(" dfdID,
                                "input clock, input ready, output done,",
                                printf "//%s (%i args)" name $ length args,
                                unlines $ map (renderArg "input" "node" True ",") (zip [0..] args),
                                "output [7:0] result",
                                ");",
                                "assign done = ready;",
                                concatMap snd . uniq $ renderNode root,
                                printf "assign result = node_%i;" $ nodeID root,
                                "endmodule\n"
                               ]

--WIP: This function renders a recursive function
--There are two trees of evaluation for a tail-recursive function:
--  -the base case, where the root is the resulting expression.
--  -the recursive case, where the root is the recursive call.
--Each element in the list of recursive cases will correspond to one of these.
--
--structure: input -> comb logic -> registers -> comb logic ...
--NOTE: The 'combinatorial' logic may include calls to synchronous functions, so it's not actually combinatorial.
renderRecursiveFunc :: DFD -> [RecursiveCase] -> String
renderRecursiveFunc (DFD dfdID name args _ _ root) recCases = res where
    res = unlines [ --Synchronous logic
                    printf "module dfd_%i(" dfdID,
                    indent [
                        printf "//%s (%i args)" name $ length args,
                        "input clock,",
                        "input ready,",
                        "output reg done,",

                        unlines $ map (renderArg "input" "inArg" False ",") (zip [0..] args),

                        "output [7:0] result",
                        ");",
                        "",
                        "wire advance, recurse;",
                        "reg running;",
                        unlines $ map (renderArg "reg" "nextArg" False ";") (zip [0..] args),
                        "",

                        let
                            inArgs  = unlines . map (renderArg "" "nextArg" False ",") $ zip [0..] args
                            outArgs = unlines . map (renderArg "" "outArg"  False ",") $ zip [0..] args
                        in printf "dfd_%i_cmb(clock, ready, advance, recurse,\n%s%s result);" dfdID inArgs outArgs,
                        "",

                        "always @(posedge clock) begin",
                        indent [
                            "if(ready ^ running)",
                            "\trunning <= ready;",
                            "",

                            "if(ready & ~running) begin",
                            --nextArgs <= inArgs
                            let
                                f i = printf "%s_%i <= %s_%i;" "nextArg" i "inArg" i
                            in  indent $ map f [0 .. length args - 1],
                            "end",
                            "",

                            "if(running) begin",
                            "\tdone <= advance & ~recurse;",
                            --nextArgs <= outArgs
                            let
                                f i = printf "%s_%i <= %s_%i;" "nextArg" i "outArg" i
                            in  indent $ map f [0 .. length args - 1],
                            "end else",
                            "\tdone <= 0;"
                        ],
                        "end"
                    ],
                    "endmodule\n",

                    --Combinatorial logic
                    printf "module dfd_%i_cmb(" dfdID,
                    indent [
                        printf "//Input args: %s (%i args)" name $ length args,
                        "input clock,",
                        "input ready,",
                        "output reg done,",
                        "output reg recurse,",
                        unlines . map (renderArg "input" "node" True ",") $ zip [0..] args,
                        unlines . map (renderArg "output reg" "outArg" False ",") $ zip [0..] args,
                        printf "output reg [7:0] result",
                        ");",
                        "",

                        --Define valid_%i, ready_%i, done_%i for each case
                        "//Control signals & logic",
                        concatMap snd . uniq $ concatMap defineRecCase $ zip [0..] recCases,
                        "",

                        --Muxing logic
                        "always @(*) begin",
                        indent . lines . concatMap ((++ "else ") . selectRecCase) $ zip [0..] recCases,
                        indent [
                        "begin",
                        indent [
                                "//This should never happen, but is needed to remove latches",
                                "recurse = 1'bX;",
                                "done = 1'bX;",
                                "result = 8'dX;"
                            ],
                            indent . lines $ concatMap nullArg [0 .. length args - 1],
                            "end"
                        ],
                        "end"
                    ],
                    "endmodule\n"
                  ]

    --Define control signals for a recursive case
    --TODO: assuming that the logic for selecting the case is combinatorial - need to add explicit check for this
    defineRecCase :: (Int, RecursiveCase) -> [VNodeDef]
    defineRecCase (i, rCase) = (-i, core) : vNodes ++ auxNodes where
        core = unlines [
            printf "wire valid_%i, ready_%i, done_%i;" i i i,
            printf "assign valid_%i = %s;" i $ joinMap " & " boolNode $ recConds rCase,
            printf "assign ready_%i = 1;" i,                                --treating case selection logic as combinatorial
            printf "assign done_%i = 1;" i,                                 --TODO: set this appropriately

            if isRecursive rCase
                then (unlines . zipWith (setArg i) [0..]) (recArgs rCase)
                else printf "wire [7:0] result_%i;\nassign result_%i = node_%i;" i i (nodeID . baseRoot $ rCase)
            ]

        vNodes = concatMap renderNode . both $ recConds rCase
        auxNodes = if isRecursive rCase
                   then concatMap renderNode $ recArgs rCase
                   else renderNode $ baseRoot rCase

    boolNode :: Either DNode DNode -> String
    boolNode (Left  n) = printf "~node_%i" $ nodeID n
    boolNode (Right n) = printf  "node_%i" $ nodeID n

    --i is the arg index, j is the recursive case index, a is the node
    setArg :: Int -> Int -> DNode -> String
    setArg j i a = printf "wire [7:0] outArg_%i_%i;\nassign outArg_%i_%i = node_%i;" j i j i $ nodeID a

    --multiplexor logic
    selectRecCase :: (Int, RecursiveCase) -> String
    selectRecCase (i, rCase) = unlines [
                                        printf "if(valid_%i) begin" i,
                                        indent [
                                            printf "recurse = %i;" $ fromEnum $ isRecursive rCase,
                                            printf "done = done_%i;" i,

                                            let
                                                setRes = printf "result = result_%i;\n" i
                                            in if isRecursive rCase
                                                then ("result = 8'hXX;\n" ++) $ (concatMap (selectArg i) [0 .. length args - 1])
                                                else (setRes ++) $ (concatMap nullArg [0 .. length args - 1])
                                        ],
                                        "end"
                                   ] where
        --i is the arg index, j is the recursive case index
        selectArg :: Int -> Int -> String
        selectArg j i = printf "outArg_%i = outArg_%i_%i;\n" j j i

--Helper method for removing latches
nullArg :: Int -> String
nullArg i  = printf "outArg_%i = 8'hXX;\n" i

--Defines an argument to a Verilog module.
--  io: the storage class. e.g. "input"/"output". Type will be omitted if storage class is blank.
--  prefix. e.g. "node".
--  useNodeId: whether the numeric identifier used will be the node ID or the argument index
--  (i, (ai, t): i is the arg index, ai is the node ID, t is the type
--  tail: a string to append to the end of the result. Useful for semicolons, etc.
renderArg :: String -> String -> Bool -> String -> (Int, (NodeId, DType)) -> String
renderArg io prefix useNodeId tail (i, (argID, t)) = printf "%s %s %s_%i%s" io hwType prefix index tail where
    hwType = if io == ""
             then ""
             else vType t
    index = if useNodeId
            then argID
            else i

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

--Helper function for indenting blocks of code
indent :: [String] -> String
indent = unlines . map f where
    f = intercalate "\n" . map ('\t':) . lines

indentN :: Int -> [String] -> String
indentN n = unlines . map f where
    f = intercalate "\n" . map (delim ++) . lines
    delim = take n (repeat '\t')

