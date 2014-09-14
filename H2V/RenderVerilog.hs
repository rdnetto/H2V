--This module contains code for rendering DFDs to Verilog
module RenderVerilog (dfdToVerilog) where

import Control.Monad
import Data.List
import Text.Printf

import Common
import DfdDef

--IDEA: implement closure support using partial application. This simplifies rewriting application, and issues with nested closures

--If there is an (undirected) loop in the graph, all nodes above it are defined and assigned multiple times.
--Therefore, we tag each node definition with a UID, to eliminate duplicate definitions.
--We store the definition and assignment separately, because Modelsim requires the definition to precede all uses.
--vModDeps contains the definition of any modules used by this node. (Needed because modules can't be nested.)
--This functional approach is cleaner, but less efficient than the monadic/stateful approach.
data VNodeDef = VNodeDef{
                    vNodeId :: NodeId,
                    vDef :: String,
                    vAssign :: String,
                    vModDeps :: String
                }

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
    | fCalls dfd dfd        = renderRecursiveFunc dfd $ recursiveCases dfd
    | otherwise             = unlines [concatMap vModDeps defs,
                                       printf "module dfd_%i(" dfdID,
                                       "input clock, input ready, output done,",
                                       printf "//%s (%i args) [dfd_%i]" name (length args) dfdID,
                                       unlines $ map (renderArg "input" "node" True ",") (zip [0..] args),
                                       "output [7:0] result",
                                       ");",
                                       concatNodes defs,
                                       printf "assign result = node_%i;" $ nodeID root,
                                       printf "assign done = node_%i_done;" $ nodeID root,
                                       "endmodule\n"
                                      ]
    where
        defs = renderNode root

--There are two trees of evaluation for a tail-recursive function:
--  -the base case, where the root is the resulting expression.
--  -the recursive case, where the root is the recursive call.
--Each element in the list of recursive cases will correspond to one of these.
--
--structure: input -> comb logic -> registers -> comb logic ...
--NOTE: The 'combinatorial' logic may include calls to synchronous functions, so it's not actually combinatorial.
renderRecursiveFunc :: DFD -> [RecursiveCase] -> String
renderRecursiveFunc (DFD dfdID name args _ _ root) recCases = res where
    res = unlines [ --Combinatorial logic
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
                        concatNodes $ concatMap defineRecCase $ zip [0..] recCases,
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
                    "endmodule\n",

                    --Synchronous logic
                    printf "module dfd_%i(" dfdID,
                    indent [
                        printf "//%s (%i args) [dfd_%i]" name (length args) dfdID,
                        "input clock,",
                        "input ready,",
                        "output done,",

                        unlines $ map (renderArg "input" "inArg" False ",") (zip [0..] args),

                        "output [7:0] result",
                        ");",
                        "",
                        "wire advance, recurse;",
                        "reg wasReady;",
                        unlines $ map (renderArg "reg" "nextArg" False ";") (zip [0..] args),
                        unlines $ map (renderArg "wire" "outArg" False ";") (zip [0..] args),
                        "",

                        let
                            inArgs  = unlines . map (renderArg "" "nextArg" False ",") $ zip [0..] args
                            outArgs = unlines . map (renderArg "" "outArg"  False ",") $ zip [0..] args
                        in printf "dfd_%i_cmb cmb(clock, wasReady, advance, recurse,\n%s%s result);" dfdID inArgs outArgs,
                        "assign done = ready & advance & ~recurse;",
                        "",

                        "always @(posedge clock) begin",
                        indent [
                            "wasReady <= ready;",
                            "",

                            "if(ready & ~wasReady) begin",
                            --nextArgs <= inArgs
                            let
                                f i = printf "%s_%i <= %s_%i;" "nextArg" i "inArg" i
                            in  indent $ map f [0 .. length args - 1],
                            "end",
                            "",

                            "if(wasReady & ~done) begin",
                            --nextArgs <= outArgs
                            let
                                f i = printf "%s_%i <= %s_%i;" "nextArg" i "outArg" i
                            in  indent $ map f [0 .. length args - 1],
                            "end"
                        ],
                        "end"
                    ],
                    "endmodule\n"
                  ]

    --Define control signals for a recursive case
    --TODO: assuming that the logic for selecting the case is combinatorial - need to add explicit check for this
    defineRecCase :: (Int, RecursiveCase) -> [VNodeDef]
    defineRecCase (i, rCase) = (VNodeDef (-i) defs assigns "") : vNodes ++ auxNodes where
        defs = unlines [
                    printfAll "wire valid_%i, ready_%i, done_%i;" i,
                    outDef
                ]

        assigns = unlines [         --TODO: set ready/done
                    printf "assign valid_%i = %s;" i $ joinMap " & " boolNode $ recConds rCase,
                    printf "assign ready_%i = 1;" i,                                --treating case selection logic as combinatorial
                    printf "assign done_%i = %i;" i $ (fromEnum . not . isRecursive) rCase,
                    outAss
                ]

        (outDef, outAss) = if isRecursive rCase
                           then map2 unlines unlines . splitTuple . zipWith (setArg i) [0..] $ recArgs rCase
                           else (printf "wire [7:0] result_%i;" i,
                                 printf "assign result_%i = node_%i;" i (nodeID . baseRoot $ rCase))

        vNodes = concatMap renderNode . both $ recConds rCase
        auxNodes = if isRecursive rCase
                   then concatMap renderNode $ recArgs rCase
                   else renderNode $ baseRoot rCase

    boolNode :: Either DNode DNode -> String
    boolNode (Left  n) = printf "~node_%i" $ nodeID n
    boolNode (Right n) = printf  "node_%i" $ nodeID n

    --i is the arg index, j is the recursive case index, a is the node
    setArg :: Int -> Int -> DNode -> (String, String)
    setArg j i a = (def, ass) where
        def = printf "wire [7:0] outArg_%i_%i;" j i
        ass = printf "assign outArg_%i_%i = node_%i;" j i $ nodeID a

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
        --outArg_i_j is argument j for recursive case i; recursive case is innermost
        selectArg :: Int -> Int -> String
        selectArg i j = printf "outArg_%i = outArg_%i_%i;\n" j i j

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
renderNode (DLiteral nodeID value) = return $ VNodeDef nodeID def ass "" where
    def = unlines [ printf "wire %s node_%i;" (vType UndefinedType) nodeID,
                    printf "wire node_%i_done;" nodeID
                  ]
    ass = unlines [ printf "assign node_%i = %i;" nodeID value,
                    printf "assign node_%i_done = 1;" nodeID
                  ]

--argument - wire/reg is defined with function, but need to manually specify ready/done signals
renderNode (DVariable nodeID _ Nothing) = return $ VNodeDef nodeID def ass "" where
    def = printf "wire node_%i_done;\n" nodeID
    ass = printf "assign node_%i_done = ready;\n" nodeID

renderNode (DVariable varID t (Just val)) = valDef ++ return (VNodeDef varID def ass "") where
    valDef = renderNode val
    def = unlines [ printf "wire %s node_%i;" (vType t) varID,
                    printf "wire node_%i_done;" varID
                  ]
    ass = unlines [ printf "assign node_%i = node_%i;" varID (nodeID val),
                    printf "assign node_%i_done = node_%i_done;" varID (nodeID val)
                  ]

renderNode (DFunctionCall appID f args)
    | dfdID f == (-1) = aDefs ++ return (renderBuiltin appID (builtinOp $ dfdRoot f) args)
    | otherwise       = aDefs ++ return (VNodeDef appID def ass "")
    where
        def = unlines [ printf "wire %s node_%i;" (vType $ returnType f) appID,
                        printf "wire node_%i_ready, node_%i_done;" appID appID
                      ]
        ass = unlines [
                        printf "assign node_%i_ready = %s;" appID ready,
                        printf "dfd_%i fcall_%i(clock, node_%i_ready, node_%i_done, %s node_%i);\n" (dfdID f) appID appID appID aAsses appID
                      ]
        ready = joinMap " & " (printf "node_%i_done" . nodeID) args
        aDefs = concatMap renderNode args
        aAsses = concatMap argEdge args

        argEdge :: DNode -> String
        argEdge a = printf "node_%i, " (nodeID a)

renderBuiltin :: NodeId -> BuiltinOp -> [DNode] -> VNodeDef
renderBuiltin resID BitwiseNot args@(arg:[]) = VNodeDef resID (def ++ ds) (ass ++ as) "" where
    def = printf "wire %s node_%i;\n" (vType $ nodeType arg) resID
    ass = printf "assign node_%i = ~node_%i;\n" resID (nodeID arg)
    (ds, as) = genericDone resID args

renderBuiltin resID (BinaryOp op) args@(a0:a1:[]) = VNodeDef resID (def ++ ds) (ass ++ as) "" where
    def = printf "wire %s node_%i;\n" (vType $ nodeType a0) resID          --TODO: should really use the larger of the two types
    ass = printf "assign node_%i = node_%i %s node_%i;\n" resID (nodeID a0) op (nodeID a1)
    (ds, as) = genericDone resID args

renderBuiltin resID Ternary args@(cond:tExp:fExp:[]) = VNodeDef resID (def ++ ds) (ass ++ as) "" where
    def = printf "wire %s node_%i;\n" (vType $ nodeType tExp) resID        --TODO: should really use the larger of the two types
    ass = printf "assign node_%i = node_%i ? node_%i : node_%i;\n" resID (nodeID cond) (nodeID tExp) (nodeID fExp)
    (ds, as) = genericDone resID args

--generates ready/done signals for builtin functions
genericDone :: NodeId -> [DNode] -> (String, String)
genericDone resID args = (def, ass) where
    def = printf "wire node_%i_done;\n" resID
    ass = printf "assign node_%i_done = %s;\n" resID $ joinMap " & " (printf "node_%i_done" . nodeID) args

--Helper function for extracting the contents of VNodeDefs
concatNodes :: [VNodeDef] -> String
concatNodes ns = defs ++ assigns where
    (defs, assigns) = foldl f ("", "") $ uniqV ns

    --lift uniq over VNodeDef
    uniqV :: [VNodeDef] -> [VNodeDef]
    uniqV = map snd . uniq . map (\v -> (vNodeId v, v))

    --Concatenate definitions and assignments
    f :: (String, String) -> VNodeDef -> (String, String)
    f (ds, as) node = (ds ++ vDef node, as ++ vAssign node)

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
 
--like printf, but replaces all %i with a single value.
printfAll :: String -> Int -> String
printfAll f i = replace "%i" (show i) f

