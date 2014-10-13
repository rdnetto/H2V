--This module contains code for rendering DFDs to Verilog
module RenderVerilog (dfdToVerilog) where

import Control.Monad
import Data.Either
import Data.List
import Text.Printf
import Debug.Trace

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
                } deriving (Show, Eq)

--A tail-recursive DFD will be a tree of IFs, where each leaf is a recursive call or a non-recursive expression.
--Returns a list of 2-tuples representing each leaf.
--The first element of the tuple is a list of conditions, where left conditions must be negated.
--    Earlier conditions have higher precedence.
--The second element of the tuple is:
--  Left: a non-recursive expression
--  Right: a list of arguments for the recursive call
--The third element of the tuple is a list of 2-tuples, which specify the index of lists which require elements and how many are required
type RecursiveCase = ([Either DNode DNode], Either DNode [DNode], [(NodeId, Int)])

--Conditions for case to be valid. Left nodes need to be negated.
recConds :: RecursiveCase -> [Either DNode DNode]
recConds (x, _, _) = x

--Returns False if the argument is a base case
isRecursive :: RecursiveCase -> Bool
isRecursive (_, x, _) = isRight x

--NodeId of root node for base case
baseRoot :: RecursiveCase -> DNode
baseRoot (_, Left x, _) = x

--Arguments for the recursive call
recArgs :: RecursiveCase -> [DNode]
recArgs (_, Right x, _) = x

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
    eCalls' (DFunctionCall _ fc _ _) = (dfdID fc == dfdID target)
    eCalls' _ = False

--Assumes function is self-recursive - use `calls` to check this.
recursiveCases :: DFD -> [RecursiveCase]
recursiveCases f = recExpr [] $ dfdRoot f where
    recExpr :: [Either DNode DNode] -> DNode -> [RecursiveCase]
    recExpr conds node
        | isIf node           = (recExpr (trueCond:conds) trueBranch) ++ (recExpr (falseCond:conds) falseBranch)
        | isFunctionCall node = return (conds', Right $ callArgs node, listConds)
        | otherwise           = return (conds', Left node, listConds)        --is expression
        where
        [cond, trueBranch, falseBranch] = callArgs node
        trueCond = Right cond
        falseCond = Left cond
        (conds', listConds) = partitionEithers $ map extractListConds conds

extractListConds :: Either DNode DNode -> Either (Either DNode DNode) (NodeId, Int)
extractListConds (Right (DFunctionCall _ DFD{dfdRoot = root} [eCount, list] _))
    | (isBuiltin root && builtinOp root == ListMinAvail) = Right (nodeID list, getConstant eCount)
extractListConds x = Left x

--Render combinatorial functions.
--TODO: add assign statements to link ready/done signals
renderFunc :: DFD -> String
renderFunc dfd@(DFD dfdID name args _ _ root)
    | fCalls dfd dfd        = renderRecursiveFunc dfd $ recursiveCases dfd
    | otherwise             = unlines [concatMap vModDeps defs,
                                       printf "module dfd_%i(" dfdID,
                                       indent [
                                           printf "//%s (%i args) [dfd_%i]" name (length args) dfdID,
                                           "input clock, input ready, output done,",
                                           concatMap (renderArg "input" "node" True "," par) (zip [0..] args),
                                           rstrip . chopComma $ renderArg "output" "node" True ", " par (0, (nodeID root, retType)),
                                       ");"
                                       ],
                                       indent $ map (\(i, _) -> printf "wire node_%i_done; //arg" i) args,
                                       indent . lines $ concatNodes defs',
                                       indent $ map (\(i, _) -> printf "assign node_%i_done = ready;" i) args,
                                       '\t' : doneAssign,
                                       "endmodule\n"
                                      ]
    where
        par = if   isList $ returnType dfd
              then getParallelism root
              else 1
        defs = renderNode root
        retType = selectType [returnType_ dfd, nodeType root]
        doneAssign = printf "assign done = node_%i_done;" $ nodeID root
        --need to filter out definitions from the root node that are already present in the module def
        argDefs = map strip . lines $ concat [ concatMap (renderArg "" "node" True "\n" par) (zip [0..] args),
                                              renderArg "" "node" True "\n" par (0, (nodeID root, retType))
                                            ]
        argIDs = map fst args
        (rootDefs, otherDefs) = partition (\n -> vNodeId n == nodeID root) defs
        defs' = (map filterRootDef rootDefs) ++ (filter (\n -> not $ vNodeId n `elem` argIDs) otherDefs)

        filterRootDef :: VNodeDef -> VNodeDef
        filterRootDef v = v{vDef = def'} where
            def = vDef v
            def' = unlines . filterMap containsArg ("//" ++) . lines $ def
            containsArg :: String -> Bool
            containsArg line = any2 (\arg suf -> isInfixOf (arg ++ suf) line) argDefs [" ", ",", ";"]

--There are two trees of evaluation for a tail-recursive function:
--  -the base case, where the root is the resulting expression.
--  -the recursive case, where the root is the recursive call.
--Each element in the list of recursive cases will correspond to one of these.
--
--structure: input -> comb logic -> registers -> comb logic ...
--NOTE: The 'combinatorial' logic may include calls to synchronous functions, so it's not actually combinatorial.
renderRecursiveFunc :: DFD -> [RecursiveCase] -> String
renderRecursiveFunc dfd@(DFD dfdID name args _ _ root) recCases
    | isList retType = renderRecursiveListFunc dfd recCases
    | otherwise      = res
    where
        retType = selectType [returnType_ dfd, nodeType root]
        res = unlines [ --Combinatorial logic
                    printf "module dfd_%i_cmb(" dfdID,
                    indent [
                        printf "//Input args: %s (%i args)" name $ length args,
                        "input clock,",
                        "input ready,",
                        "output reg done,",
                        "output reg recurse,",
                        unlines . map (renderArg "input" "node" True "," 1) $ zip [0..] args,
                        unlines . map (renderArg "output reg" "outArg" False "," 1) $ zip [0..] args,
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

                        unlines $ map (renderArg "input" "inArg" False "," 1) (zip [0..] args),

                        "output [7:0] result",
                        ");",
                        "",
                        "wire advance, recurse;",
                        "reg wasReady;",
                        unlines $ map (renderArg "reg" "nextArg" False ";" 1) (zip [0..] args),
                        unlines $ map (renderArg "wire" "outArg" False ";" 1) (zip [0..] args),
                        "",

                        let
                            inArgs  = unlines . map (renderArg "" "nextArg" False "," 1) $ zip [0..] args
                            outArgs = unlines . map (renderArg "" "outArg"  False "," 1) $ zip [0..] args
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

--WIP: implement
renderRecursiveListFunc :: DFD -> [RecursiveCase] -> String
renderRecursiveListFunc dfd@(DFD dfdID name args _ _ root) recCases = ""

--Helper method for removing latches
nullArg :: Int -> String
nullArg i  = printf "outArg_%i = 8'hXX;\n" i

--Defines the wires for a node, but doesn't connect them to anything
defineNode :: NodeId -> DType -> Int -> String
defineNode nodeID t par = defineNodeX (printf "node_%i" nodeID) t par

defineNodeX :: String -> DType -> Int -> String
defineNodeX label (DList t) par = res where
    res = unlines [ printf "wire %s_req, %s_ack;" label label,
                    unlines . parEdge par $ printf "wire %s %s_value_%i;" (scalarVType t) label,
                    unlines . parEdge par $ printf "wire %s_value_%i_valid;" label,
                    printf "wire %s_done;" label
                  ]

defineNodeX label t _ = unlines [line1, line2] where
    line1 = printf "wire %s %s;" (scalarVType t) label
    line2 = printf "wire %s_done;" label

--Generates the assign statements needed to connect two nodes. LHS is set to RHS.
assignNode :: DNode -> DNode -> String
assignNode lhs rhs
    | isList (nodeType lhs) || isList (nodeType rhs) = unlines res
    where
        par = getParallelism rhs
        lhsID = nodeID lhs
        rhsID = nodeID rhs
        res = [ printf "assign node_%i_req = node_%i_req;" rhsID lhsID,
                printf "assign node_%i_ack = node_%i_ack;" lhsID rhsID,
                unlines . parEdge par $ \i -> printf "assign node_%i_value_%i = node_%i_value_%i;" lhsID rhsID i i,
                unlines . parEdge par $ \i -> printf "assign node_%i_value_%i_valid = node_%i_value_%i_valid;" lhsID rhsID i i,
                printf "assign node_%i_done = node_%i_done;" lhsID rhsID
              ]

assignNode lhs rhs = unlines [line1, line2] where
    line1 = printf "assign node_%i = node_%i;" (nodeID lhs) (nodeID rhs)
    line2 = printf "assign node_%i_done = node_%i_done;" (nodeID lhs) (nodeID rhs)

--Defines an argument to a Verilog module.
--  io: the storage class. e.g. "input"/"output". Type will be omitted if storage class is blank.
--  prefix. e.g. "node".
--  useNodeId: whether the numeric identifier used will be the node ID or the argument index
--  par: the degree of parallelism, if the argument is a list
--  (i, (ai, t): i is the arg index, ai is the node ID, t is the type
--  tail: a string to append to the end of the result. Useful for semicolons, etc.
renderArg :: String -> String -> Bool -> String -> Int -> (Int, (NodeId, DType)) -> String
renderArg io prefix useNodeId tail par (i, (argID, DList t)) = concat lines where
    lines = [ printf "%s %s_%i_req%s"         invIo    prefix index tail,
              printf "%s %s_%i_ack%s"         io prefix index tail
            ] ++ parEdge par (\pI -> printf "%s %s %s_%i_value_%i%s"    io hwType prefix index pI tail)
              ++ parEdge par (\pI -> printf "%s %s_%i_value_%i_valid%s" io prefix index pI tail)
    hwType = if io == ""
             then ""
             else scalarVType t
    index = if useNodeId
            then argID
            else i
    invIo = maybe io id (lookup io [("input", "output"), ("output", "input")])

renderArg io prefix useNodeId tail _ (i, (argID, t)) = printf "%s %s %s_%i%s" io hwType prefix index tail where
    hwType = if io == ""
             then ""
             else scalarVType t
    index = if useNodeId
            then argID
            else i

renderNode :: DNode -> [VNodeDef]
renderNode (DLiteral nodeID value) = return $ VNodeDef nodeID def ass "" where
    def = defineNode nodeID UndefinedType 1
    ass = unlines [ printf "assign node_%i = %i;" nodeID value,
                    printf "assign node_%i_done = ready;" nodeID
                  ]

--arguments which are lists are already defined
renderNode (DArgument nodeID (DList _) _) = []

--argument - wire/reg is defined with function, but need to manually specify ready/done signals
renderNode (DArgument nodeID _ _) = return $ VNodeDef nodeID def ass "" where
    def = printf "wire node_%i_done;\n" nodeID
    ass = printf "assign node_%i_done = ready;\n" nodeID

renderNode var@(DVariable varID t val) = valDef ++ return (VNodeDef varID def ass "") where
    valDef = renderNode val
    def = defineNode varID t $ getParallelism val
    ass = assignNode var val

renderNode (DFunctionCall appID f args p)
    | dfdID f == (-1) = aDefs ++ return (renderBuiltin appID (builtinOp $ dfdRoot f) par args)
    | otherwise       = aDefs ++ return (VNodeDef appID def ass "")
    where
        par = parValue p
        def = unlines [ defineNode appID (returnType f) par,
                        printf "wire node_%i_ready;" appID
                      ]
        ass = unlines [ printf "assign node_%i_ready = %s;" appID ready,
                        printf "dfd_%i fcall_%i(clock, node_%i_ready, node_%i_done, %s %s);\n" fID appID appID appID aAsses resAss
                      ]
        fID = dfdID f
        args' = if   isBuiltinMacro $ dfdRoot f                                          --don't render the lambda
                then tail args
                else args
        ready = joinMap " & " (printf "node_%i_done" . nodeID) args'
        aDefs = concatMap renderNode args'
        aAsses = concatMap (argEdge par) args'
        resAss = chopComma . argEdge par $ DArgument appID (returnType f) p

renderNode elem@(DTupleElem elemID tupleIndex tuple) = (renderNode tuple) ++ return (VNodeDef elemID def ass "") where
    par = getParallelism tuple
    tupleID = nodeID tuple
    def = defineNode elemID (nodeType elem) par
    ass = unlines $ case tupleIndex of
                     0 -> [ printf "assign node_%i = node_head_%i;" elemID tupleID,
                            printf "assign node_%i_done = node_%i_done;" elemID tupleID
                          ]
                     1 -> [ printf "assign node_tail_%i_req = node_%i_req;" tupleID elemID,
                            printf "assign node_%i_ack = node_tail_%i_ack;" elemID tupleID,
                            unlines . parEdge par $ \i -> printf "assign node_%i_value_%i = node_tail_%i_value_%i;" elemID i tupleID i,
                            unlines . parEdge par $ \i -> printf "assign node_%i_value_%i_valid = node_tail_%i_value_%i_valid;" elemID i tupleID i,
                            printf "assign node_%i_done = node_%i_done;" elemID tupleID
                          ]
                     2 -> [ printf "assign node_%i = node_head_%i_valid;" elemID tupleID,
                            printf "assign node_%i_done = node_%i_done;" elemID tupleID
                          ]
                     _ -> error $ "Invalid tuple index: " ++ show tupleIndex

--List literals are handled by generating a module to implement the list interface
renderNode (DListLiteral listID items p) = (VNodeDef listID def ass mod):elemDefs where
    par = parValue p
    def = unlines [ defineNode listID (DList UndefinedType) par,
                    printf "wire node_%i_ready;" listID
                  ]
    ass = unlines [ printf "assign node_%i_ready = %s;" listID readySig,
                    printfAll "listLiteral_%i listLit_%i(clock, node_%i_ready," listID,
                    chopComma $ indent [
                        concatMap (argEdge par) items,
                        argEdge par (DArgument listID (DList UndefinedType) p)
                    ],
                    ");",
                    genericDone listID items
                  ]
    readySig = joinMap " & " (printf "node_%i_done" . nodeID) items
    elemDefs = concatMap renderNode items
    elemIndices = [0 .. length items - 1]
    mod = if   items == []
          then emptyMod
          else mod'

    emptyMod = unlines [
                    printf "module listLiteral_%i(" listID,
                    indent [
                        "input clock,",
                        "input ready,",
                        "input req,",
                        "output reg ack,",
                        unlines . parEdge par $ printf "output [7:0] value_%i,",
                        chopComma . unlines . parEdge par $ printf "output value_%i_valid,"
                    ],
                    ");\n",
                    indent [
                        "reg lastReq;",
                        "",
                        unlines . parEdge par $ printf "assign value_%i = 8'hXX;",
                        chopComma . unlines . parEdge par $ printf "assign value_%i_valid = 0;",
                        "",
                        "always @(posedge clock) begin",
                        "\tack <= ready & ~lastReq & req;",
                        "\tlastReq <= req;",
                        "end"
                        ],
                    "endmodule\n"
                ]

    parCases = length items `div` par
    mod' = unlines [printf "module listLiteral_%i(" listID,
                    indent [
                        "input clock,",
                        "input ready,",
                        unlines $ map (printf "input [7:0] x_%i,") elemIndices,
                        "input req,",
                        "output reg ack,",
                      unlines . parEdge par $ printf "output reg [7:0] value_%i,",
                      chopComma . unlines . parEdge par $ printf "output reg value_%i_valid,"
                    ],
                    ");\n",
                    indent [
                        "reg dummy;",
                        "reg lastReq;",
                        "reg [7:0] index;",
                        "",

                        "always @(*) begin",
                        indent [
                            "case (index)",
                            indent [
                                concatMap elemCase [0 .. parCases],
                                "default: begin",
                                indent . parEdge par $ printf "value_%i = 8'hFF;",
                                chopComma . indent . parEdge par $ printf "value_%i_valid = 0;",
                                "end"
                            ],
                            "endcase"
                        ],
                        "end\n",

                        "always @(posedge clock) begin",
                        indent [
                            "lastReq <= req;\n",

                            "if(~ready) begin",
                            indent [
                                "index <= 8'hFF;",
                                "ack <= 0;"
                            ],
                            "end else if(req & ~lastReq) begin",
                            indent [
                                printf "if(index < 8'd%i || index == 8'hFF) begin" $ parCases + 1,
                                "\tindex <= index + 1;",
                                "end",
                                "ack <= 1;"
                            ],
                            "end else begin",
                            indent [
                                "ack <= 0;"
                            ],
                            "end"
                        ],
                        "end"
                    ],
                    "endmodule\n"
                  ]

    --i is the index of the iteration - value_0 will be set to par*i
    elemCase :: Int -> String
    elemCase i = res where
        res = unlines [
                printf "%i: begin" i,
                indent $ parEdge par valAssign,
                "end"
              ]
        offset = par*i

        valAssign :: Int -> String
        valAssign j
            | isValid   = printf fmt1 j (offset+j) j
            | otherwise = printf fmt2 j j
            where
                isValid = (offset + j) < (length items)
                fmt1 = unlines [
                        "value_%i = x_%i;",
                        "value_%i_valid = 1'b1;"
                       ]
                fmt2 = unlines [
                        "value_%i = 8'hFF;",
                        "value_%i_valid = 1'b0;"
                       ]

renderNode n = error $ "Unable to render:\n" ++ (show n)

argEdge :: Int -> DNode -> String
argEdge p n = argEdgeX "node" n p

argEdgeX :: String -> DNode -> Int -> String
argEdgeX lbl a p = renderArg "" lbl True ", " p (0, (nodeID a, nodeType a))

--Helper method for implementing the parallel list interface
parEdge :: Int -> (Int -> String) -> [String]
parEdge par f = map f [0 .. par - 1]

--Returns the parallelism of an arbitrary node, traversing the DFD if necessary
getParallelism :: DNode -> Int
getParallelism node
    | hasParallelism node = parValue $ parallelism node
getParallelism (DVariable _ _ value) = getParallelism value
getParallelism node@DArgument{} = parValue $ parallelism node
getParallelism node@DFunctionCall{} = parValue $ parallelism node
getParallelism node@DTupleElem{} = getParallelism $ tuple node
getParallelism node = error $ "getParallelism: unknown node " ++ show node

renderBuiltin :: NodeId -> BuiltinOp -> Int -> [DNode] -> VNodeDef
renderBuiltin resID BitwiseNot _ args@(arg:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    def = defineNode resID (nodeType arg) 1
    ass = printf "assign node_%i = ~node_%i;\n" resID (nodeID arg)
    doneAs = genericDone resID args

renderBuiltin resID (BinaryOp ":") par args@(a0:a1:[])
    | par == 1  = VNodeDef resID def (ass ++ doneAs) ""
    | otherwise = error "Cons does not support parallel access yet."
    where
        def = defineNode resID (nodeType a1) par
        ass = concat [
                printf "Cons cons_%i(clock, node_%i_done, node_%i, " resID resID a0ID,
                argEdge par (DArgument  a1ID (DList UndefinedType) (InferredPar par)),
                argEdge par (DArgument resID (DList UndefinedType) (InferredPar par)),          --TODO: implement parallelism
                ");\n"
            ]
        a0ID = nodeID a0
        a1ID = nodeID a1
        doneAs = genericDone resID args

renderBuiltin resID (BinaryOp "++") par args@(a0:a1:[])
    | par == 1  = VNodeDef resID def ass ""
    | otherwise = error "Concat does not support parallel access yet."
    where
        def = defineNode resID (nodeType a0) par
        a0ID = nodeID a0
        a1ID = nodeID a1
        ass = unlines [
                printf "Concat concat_%i(clock, node_%i_done," resID resID,
                argEdge par (DArgument  a0ID (DList UndefinedType) (InferredPar par)),          --TODO: implement parallelism
                argEdge par (DArgument  a1ID (DList UndefinedType) (InferredPar par)),
                argEdge par (DArgument resID (DList UndefinedType) (InferredPar par)),
                ");",
                printf "assign node_%i_done = node_%i_done & node_%i_done;" resID a0ID a1ID
            ]

renderBuiltin resID (BinaryOp op) _ args@(a0:a1:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    def = defineNode resID (nodeType a0) 1
    ass = printf "assign node_%i = node_%i %s node_%i;\n" resID (nodeID a0) op (nodeID a1)
    doneAs = genericDone resID args

renderBuiltin resID Ternary par args@(cond:tExp:fExp:[]) = VNodeDef resID def (ass ++ doneAs) "" where
        resType = headOr UndefinedType $ filter (/= UndefinedType) [nodeType tExp, nodeType fExp]
        def = defineNode resID (nodeType tExp) par
        ass = case resType of
                DList _ -> listAss
                _       -> scalarAss
        [cID, tID, fID] = map nodeID [cond, tExp, fExp]
        scalarAss = printf "assign node_%i = node_%i ? node_%i : node_%i;\n" resID cID tID fID
        listTerms = map (++ ", ") ["node_%i_req", "node_%i_ack", "node_%i_value", "node_%i_value_valid"]
        listAss = concat [ printf "ListMux lm_%i(node_%i_done, node_%i," resID cID cID,
                           chopComma $ concatMap genMuxLine [resID, tID, fID],
                           ");\n"
                         ]
        genMuxLine s = rstrip $ "\n\t" ++ concatMap (\fmt -> printf fmt s) listTerms
        doneAs = genericDone resID args

renderBuiltin resID EnumList par args@(min:step:max:[]) = VNodeDef resID def (ass ++ doneAs) mod where
    --bounded
    def = defineNode resID (DList UndefinedType) par
    (ass, mod) = renderListGen resID min step (Just max) par
    doneAs = genericDone resID [min, step, max]

renderBuiltin resID EnumList par args@(min:step:[]) = VNodeDef resID def (ass ++ doneAs) mod where
    --unbounded
    def = defineNode resID (DList UndefinedType) par
    (ass, mod) = renderListGen resID min step Nothing par
    doneAs = genericDone resID [min, step]

renderBuiltin resID Decons par [list]
    | par == 1 = VNodeDef resID def ass ""
    | otherwise = error "Decons does not support parallel access yet."
    where
        listID = nodeID list
        def = concat [ defineNodeX (printf "node_head_%i" resID) UndefinedType 1,
                       defineNodeX (printf "node_tail_%i" resID) (DList UndefinedType) par,
                       printf "wire node_head_%i_valid;\n" resID,
                       printf "wire node_%i_done;\n" resID
                     ]

        ass = concat [ printf   "Decons decons_%i(clock, node_%i_done, node_%i_done,\n\t" listID listID resID,
                       strip  $ argEdge par list,
                       "\n\t",
                       lstrip $ argEdgeX "node_head" (DArgument resID UndefinedType NoPar) 1,
                       lstrip $ printf   "node_head_%i_valid,\n\t" resID,
                       lstrip . chopComma $ argEdgeX "node_tail" (DArgument resID (DList UndefinedType) (InferredPar par)) par,
                       ");\n"
                     ]

renderBuiltin resID MapMacro par [lambda, list] = VNodeDef resID def ass mod where
    listID = nodeID list
    listType = nodeType list
    f = functionCalled lambda
    fID = dfdID f
    def = defineNode resID listType par
    ass = concat [
            printf "Map_%i map_%i(clock, node_%i_done, node_%i_done,\n\t" resID resID listID resID,
            lstrip $ argEdge par list,
            "\n\t",
            lstrip . chopComma . argEdge par $ DArgument resID listType (InferredPar par),
            ");\n"
          ]
    inputType = scalarVType . snd . head $ dfdArgs f
    outputType = scalarVType $ returnType f
    mod = unlines [
              printf "module Map_%i(" resID,
              indent [
                  --referring to args as list_0 and list_1 for input and output lists, respectively
                  "input clock, input ready, output done,",
                  "output listIn_req_actual,",
                  "input  listIn_ack,",
                  unlines . parEdge par $ printf "input %s listIn_value_%i," inputType,
                  unlines . parEdge par $ printf "input       listIn_value_%i_valid,",

                  "input            listOut_req,",
                  "output reg       listOut_ack,",
                  unlines . parEdge par $ printf "output reg %s listOut_value_%i," outputType,
                  chopComma . unlines . parEdge par $ printf "output reg       listOut_value_%i_valid,",
                  ");",
                  "",

                  --waitingForInput:  waiting for a new value from listIn
                  --processingValues: waiting for f(listIn_value) to complete
                  --endOfInput:       the input list has been exhausted
                  --funcStalling:     the function is stalling, waiting for the consumer to take the next value
                  --consumerWaiting:  the consumer of the output list is waiting for a value
                  --consumerServed:   the consumer of the output list has been given a value
                  "reg wasReady;",
                  "reg listIn_req;",
                  "reg waitingForInput, processingValuesActual, endOfInput;",
                  "reg consumerServed;",
                  "wire consumerWaiting, processingValues, valuesProcessed, funcStalling;",
                  printf "wire %s %s;" outputType $ joinMap ", " id $ parEdge par (printf "nextVal_%i"),
                  printf "wire %s;" $ joinMap ", " id $ parEdge par (printf "nextVal_%i_valid"),
                  printf "wire %s;" $ joinMap ", " id $ parEdge par (printf "lambda_%i_done"),
                  unlines . parEdge par $ \i -> printf "dfd_%i lambda_%i(clock, processingValues, lambda_%i_done, listIn_value_%i, nextVal_%i);" fID i i i i,
                  "",
                  "assign done = ready;",
                  "assign listIn_req_actual = listIn_req | (ready & ~wasReady);",
                  "assign consumerWaiting = listOut_req & ~consumerServed;",
                  "assign funcStalling = ~waitingForInput & ~processingValues & ~endOfInput;",
                  "assign processingValues = processingValuesActual | (waitingForInput & listIn_ack & ~endOfInput);",
                  printf "assign valuesProcessed = %s;" . joinMap " & " id . parEdge par $ printf "lambda_%i_done",
                  "",

                  "always @(posedge clock) begin",
                  indent [
                      "wasReady <= ready;",
                      "",
                      "if(ready) begin",
                      indent [
                          "if(~wasReady | (funcStalling & consumerServed)) begin",
                          "\tlistIn_req <= 1;",
                          "\twaitingForInput <= 1'b1;",
                          "end",
                          "",
                          "if(waitingForInput & listIn_ack) begin",
                          indent [
                              "waitingForInput <= 1'b0;",
                              "",
                              printf "if(listIn_value_0_valid) begin",
                              "\tprocessingValuesActual <= 1'b1;",
                              "end else begin",
                              "\tendOfInput <= 1'b1;",
                              "end"
                          ],
                          "end",
                          "",
                          "if((valuesProcessed | endOfInput) & consumerWaiting) begin",
                          indent [
                              unlines . parEdge par $ printfAll "listOut_value_%i <= nextVal_%i;",
                              unlines . parEdge par $ printfAll "listOut_value_%i_valid <= listIn_value_%i_valid & ~endOfInput;",

                              "listOut_ack <= 1'b1;",
                              "consumerServed <= 1'b1;",
                              "if(~endOfInput) begin",
                              indent [
                                  "if (listIn_req) begin",
                                  "\tlistIn_req <= 1'b0;",
                                  "\tprocessingValuesActual <= 1'b0;",
                                  "end else begin",
                                  "\tlistIn_req <= 1'b1;",
                                  "\twaitingForInput <= 1'b1;",
                                  "end"
                              ],
                              "end"
                          ],
                          "end",
                          "",
                          "if(consumerServed)",
                          "\tlistOut_ack <= 1'b0;",
                          "",
                          "if(~listOut_req)",
                          "\tconsumerServed <= 1'b0;",
                          ""
                      ],
                      "end else begin",
                      indent [
                          "waitingForInput <= 1'b0;",
                          "processingValuesActual <= 1'b0;",
                          "endOfInput <= 1'b0;",
                          "consumerServed <= 1'b0;",
                          "listIn_req <= 1'b0;",
                          "listOut_ack <= 1'b0;",
                          unlines . parEdge par $ printf "listOut_value_%i <= 8'hFF;",
                          unlines . parEdge par $ printf "listOut_value_%i_valid <= 1'b0;"
                      ],
                      "end"
                  ],
                  "end"
              ],
              "endmodule"
          ]

renderBuiltin resID ZipMacro par [lambda, list1, list2] = VNodeDef resID def ass mod where
    list1ID = nodeID list1
    list2ID = nodeID list2
    listType = nodeType list1
    f = functionCalled lambda
    fID = dfdID f
    def = defineNode resID listType par
    ass = concat [
            printf "Zip_%i zip_%i(clock, node_%i_done & node_%i_done, node_%i_done,\n\t" resID resID list1ID list2ID resID,
            lstrip $ argEdge par list1,
            lstrip $ argEdge par list2,
            "\n\t",
            lstrip . chopComma . argEdge par $ DArgument resID listType (InferredPar par),
            ");\n"
          ]
    inputType = scalarVType . snd . head $ dfdArgs f
    outputType = scalarVType $ returnType f
    mod = unlines [
              printf "module Zip_%i(" resID,
              indent [
                  --referring to args as list_0 and list_1 for input and output lists, respectively
                  "input clock, input ready, output done,",
                  "output list1In_req_actual,",
                  "input  list1In_ack,",
                  unlines . parEdge par $ printf "input %s list1In_value_%i," inputType,
                  unlines . parEdge par $ printf "input       list1In_value_%i_valid,",

                  "output list2In_req_actual,",
                  "input  list2In_ack,",
                  unlines . parEdge par $ printf "input %s list2In_value_%i," inputType,
                  unlines . parEdge par $ printf "input       list2In_value_%i_valid,",

                  "input            listOut_req,",
                  "output reg       listOut_ack,",
                  unlines . parEdge par $ printf "output reg %s listOut_value_%i," outputType,
                  chopComma . unlines . parEdge par $ printf "output reg       listOut_value_%i_valid,",
                  ");",
                  "",

                  --waitingForInput:  waiting for a new value from listIn
                  --processingValues: waiting for f(listIn_value) to complete
                  --endOfInput:       the input list has been exhausted
                  --funcStalling:     the function is stalling, waiting for the consumer to take the next value
                  --consumerWaiting:  the consumer of the output list is waiting for a value
                  --consumerServed:   the consumer of the output list has been given a value
                  --bothAckd:         both lists have asserted ACK
                  "reg wasReady;",
                  "reg list1In_req, list2In_req, list1In_ackd, list2In_ackd;",
                  "reg waitingForInput, processingValuesActual, endOfInput;",
                  "reg consumerServed;",
                  "wire consumerWaiting, processingValues, valuesProcessed, funcStalling, bothAckd;",
                  printf "wire %s %s;" outputType $ joinMap ", " id $ parEdge par (printf "nextVal_%i"),
                  printf "wire %s;" $ joinMap ", " id $ parEdge par (printf "nextVal_%i_valid"),
                  printf "wire %s;" $ joinMap ", " id $ parEdge par (printf "lambda_%i_done"),
                  unlines . parEdge par $ \i -> printf "dfd_%i lambda_%i(clock, processingValues, lambda_%i_done, list1In_value_%i, list2In_value_%i, nextVal_%i);" fID i i i i i,
                  "",
                  "assign done = ready;",
                  "assign list1In_req_actual = list1In_req | (ready & ~wasReady);",
                  "assign list2In_req_actual = list2In_req | (ready & ~wasReady);",
                  "assign consumerWaiting = listOut_req & ~consumerServed;",
                  "assign funcStalling = ~waitingForInput & ~processingValues & ~endOfInput;",
                  "assign processingValues = processingValuesActual | (waitingForInput & bothAckd & ~endOfInput);",
                  "assign bothAckd = (list1In_ackd | list1In_ack) & (list2In_ackd | list2In_ack);",
                  printf "assign valuesProcessed = %s;" . joinMap " & " id . parEdge par $ printf "lambda_%i_done",
                  "",

                  "always @(posedge clock) begin",
                  indent [
                      "wasReady <= ready;",
                      "",
                      "if(ready) begin",
                      indent [
                          "if(~wasReady | (funcStalling & consumerServed)) begin",
                          "\tlist1In_req <= 1;",
                          "\tlist2In_req <= 1;",
                          "\twaitingForInput <= 1'b1;",
                          "end",
                          "",
                          "if(waitingForInput) begin",
                          indent [
                              "if(bothAckd) begin",
                              indent [
                                  "waitingForInput <= 1'b0;",
                                  "list1In_ackd <= 1'b0;",
                                  "list2In_ackd <= 1'b0;",
                                  "",
                                  printf "if(list1In_value_0_valid && list2In_value_0_valid) begin",
                                  "\tprocessingValuesActual <= 1'b1;",
                                  "end else begin",
                                  "\tendOfInput <= 1'b1;",
                                  "end"
                              ],
                              "end else if(list1In_ack) begin",
                              "\tlist1In_ackd <= 1'b1;",
                              "end else if(list2In_ack) begin",
                              "\tlist2In_ackd <= 1'b1;",
                              "end"
                          ],
                          "end",
                          "",
                          "if((valuesProcessed | endOfInput) & consumerWaiting) begin",
                          indent [
                              unlines . parEdge par $ printfAll "listOut_value_%i <= nextVal_%i;",
                              unlines . parEdge par $ printfAll "listOut_value_%i_valid <= list1In_value_%i_valid & list2In_value_%i_valid & ~endOfInput;",

                              "listOut_ack <= 1'b1;",
                              "consumerServed <= 1'b1;",
                              "if(~endOfInput) begin",
                              indent [
                                  "if (list1In_req) begin",
                                  "\tlist1In_req <= 1'b0;",
                                  "\tlist2In_req <= 1'b0;",
                                  "\tprocessingValuesActual <= 1'b0;",
                                  "end else begin",
                                  "\tlist1In_req <= 1'b1;",
                                  "\tlist2In_req <= 1'b1;",
                                  "\twaitingForInput <= 1'b1;",
                                  "end"
                              ],
                              "end"
                          ],
                          "end",
                          "",
                          "if(consumerServed)",
                          "\tlistOut_ack <= 1'b0;",
                          "",
                          "if(~listOut_req)",
                          "\tconsumerServed <= 1'b0;",
                          ""
                      ],
                      "end else begin",
                      indent [
                          "waitingForInput <= 1'b0;",
                          "processingValuesActual <= 1'b0;",
                          "endOfInput <= 1'b0;",
                          "consumerServed <= 1'b0;",
                          "list1In_req <= 1'b0;",
                          "list2In_req <= 1'b0;",
                          "listOut_ack <= 1'b0;",
                          unlines . parEdge par $ printf "listOut_value_%i <= 8'hFF;",
                          unlines . parEdge par $ printf "listOut_value_%i_valid <= 1'b0;"
                      ],
                      "end"
                  ],
                  "end"
              ],
              "endmodule"
          ]

renderBuiltin resID FoldMacro par [lambda, identity, list] = VNodeDef resID def ass vMod where
    listID = nodeID list
    listType = nodeType list
    f = functionCalled lambda
    fID = dfdID f
    def = unlines [
            defineNode resID listType par,
            printf "wire node_%i_ready;" resID
          ]
    ass = concat [
            printf "assign node_%i_ready = node_%i_done & node_%i_done;\n" resID listID (nodeID identity),
            printf "Fold_%i fold_%i(clock, node_%i_ready, node_%i_done,\n\t" resID resID resID resID,
            lstrip $ argEdge 1   identity,
            lstrip $ argEdge par list,
            "\n\t",
            lstrip . chopComma . argEdge 1 $ DArgument resID (returnType f) (InferredPar par),
            ");\n"
          ]
    inputType  = scalarVType . snd . head $ dfdArgs f
    outputType = scalarVType $ returnType f
    accNo = (par + 1) + ((par + 1) `mod` 2)         --no. of accumulators must be even, and we need an extra acc. for the carry
    funcNo = accNo `div` 2                                              --no. of function instances
    stageNo :: Int = ceiling . logBase 2 $ fromIntegral accNo           --no. of stages needed
    vMod = unlines [
            printf "module Fold_%i(input clock, input ready, output reg done," resID,
            indent [
                printf "input %s identity," inputType,

                "output listIn_req_actual,",
                "input  listIn_ack,",
                unlines . parEdge par $ printf "input %s listIn_value_%i," inputType,
                unlines . parEdge par $ printf "input       listIn_value_%i_valid,",

                printf "output %s result" outputType,
                ");",
                "",

                --waitingForInput:  waiting for data from list
                --processingValues: waiting for functions to complete
                --resettingFuncs:   functions have completed, ready is unasserted
                --carryPresent:     whether there is a carry value present from the previous row
                "reg wasReady, waitingForInput, processingValues, resettingFuncs;",
                "reg [3:0] foldStage;",     --2^4 stages => 2^2^4 (256) degrees of parallelism
                "reg carryPresent;",
                unlines . parEdge accNo  $ printfAll "reg [7:0] accumulator_%i_reg, accumulator_%i;",
                unlines . parEdge funcNo $ printf "wire func_%i_done;",
                unlines . parEdge funcNo $ printf "reg  func_%i_enabled;",
                unlines . parEdge funcNo $ printf "wire %s func_%i_result;" outputType,
                "wire funcs_done;",

                "assign listIn_req_actual = waitingForInput | (ready & ~wasReady);",
                "assign result = accumulator_0;",
                printf "assign funcs_done = %s;" $ joinMap " & " (printfAll "(func_%i_done | ~func_%i_enabled)") [0 .. funcNo - 1],

                --Define dummy wires to simplify the logic for setting the accumulators
                --These are needed because we have more accumulators than list values
                "//Dummy elements to simplify accumulator logic",
                let f i = unlines [
                            printf "wire listIn_value_%i_valid;" i,
                            printf "wire %s listIn_value_%i;" outputType i,
                            printf "assign listIn_value_%i_valid = 1'b0;" i,
                            printf "assign listIn_value_%i = identity;" i
                        ]
                in  unlines $ map f [par .. accNo - 1],
                "",

                let f :: Int -> String
                    f i = printf fmt fID i i i (2*i) (2*i + 1) i where
                    fmt = "dfd_%i func_%i(clock, (processingValues | listIn_ack) & func_%i_enabled, func_%i_done, accumulator_%i, accumulator_%i, func_%i_result);"
                in  unlines $ parEdge funcNo f,
                "",

                "always @(*) begin",
                indent [
                    "if(listIn_ack) begin",
                    indent [
                        "if(carryPresent) begin",
                        indent . return $ "accumulator_0 = accumulator_0_reg;",
                        indent . (flip map) [1 .. accNo-1] $ \i -> printf "accumulator_%i = listIn_value_%i;" i (i-1),
                        "end else begin",
                        indent . parEdge accNo $ printfAll "accumulator_%i = listIn_value_%i;",
                        "end"
                    ],
                    "",

                    "end else begin",
                    indent . parEdge accNo $ printfAll "accumulator_%i = accumulator_%i_reg;",
                    "end"
                ],
                "end",
                "",

                "always @(posedge clock) begin",
                indent [
                    "wasReady <= ready;",
                    "",
                    "if(ready) begin",
                    indent [
                        "if(~wasReady) begin",
                        indent [
                            "waitingForInput <= 1'b1;"
                        ],
                        "end",
                        "",

                        "if(listIn_ack & waitingForInput) begin",
                        indent [
                            "waitingForInput <= 1'b0;",
                            "",
                            printf "if(listIn_value_0_valid) begin",
                            indent [
                                "processingValues <= 1'b1;",
                                "foldStage <= 4'd0;",
                                "",

                                --load list values into accumulators
                                "if(carryPresent) begin",
                                let fmt = "accumulator_%i_reg <= (listIn_value_%i_valid ? listIn_value_%i : identity);"
                                in  indent . parEdge (accNo - 1) $ \i -> printf fmt (i+1) i i,

                                "\tfunc_0_enabled <= listIn_value_0_valid;",
                                let fmt = "func_%i_enabled <= listIn_value_%i_valid & listIn_value_%i_valid;"
                                in  indent . (flip map) [1 .. funcNo - 1] $ \i -> printf fmt i (2*i-1) (2*i),

                                "end else begin",
                                let fmt = "accumulator_%i_reg <= (listIn_value_%i_valid ? listIn_value_%i : identity);"
                                in  indent . parEdge accNo $ printfAll fmt,

                                let fmt = "func_%i_enabled <= listIn_value_%i_valid & listIn_value_%i_valid;"
                                in  indent . parEdge funcNo $ \i -> printf fmt i (2*i) (2*i+1),
                                "end"
                            ],
                            "end else begin",
                            "   done <= 1'b1;",
                            "end"
                        ],
                        "end",
                        "",

                        "if(processingValues & funcs_done) begin",
                        indent [
                            "processingValues <= 1'b0;",
                            "",
                            "if(~func_0_enabled) begin",
                            "   waitingForInput <= 1'b1;",
                            "   carryPresent <= 1'b1;",
                            "   accumulator_0_reg <= func_0_result;",
                            "end else begin",
                            "   resettingFuncs <= 1'b1;",
                            "   foldStage <= foldStage + 4'd1;",
                            "",

                            --Shift values. If a func is disabled, then take its left arg (either a single value or an identity)
                            let fmt = "accumulator_%i_reg <= (func_%i_enabled ? func_%i_result : accumulator_%i_reg);"
                            in  indent . parEdge funcNo $ \i -> printf fmt i i i (2*i),

                            indent . (flip map) [funcNo .. accNo - 1] $ printf "accumulator_%i_reg <= identity;",

                            let funcEnabled :: Int -> String
                                funcEnabled i
                                    | i < funcNo = printf "func_%i_enabled" i
                                    | otherwise  = "1'b0"
                                fmt = "%s <= %s & %s;"
                            in  indent . parEdge funcNo $ \i -> printf fmt (funcEnabled i) (funcEnabled $ 2*i) (funcEnabled $ 2*i+1),
                            "end"
                        ],
                        "end",
                        "",

                        "if(resettingFuncs) begin",
                        indent [
                            "resettingFuncs <= 1'b0;",
                            "processingValues <= 1'b1;"
                        ],
                        "end"
                    ],

                    "end else begin",
                    indent [
                        "waitingForInput <= 1'b0;",
                        "processingValues <= 1'b0;",
                        "resettingFuncs <= 1'b0;",
                        "done <= 1'b0;",
                        "carryPresent <= 1'b0;",
                        "foldStage <= 4'hX;",
                        unlines . parEdge accNo $ printf "accumulator_%i_reg <= 8'hFF;"
                    ],
                    "end"
                ],
                "end"
            ],
            "endmodule",
            ""
        ]

--Generates assign statement for bounded and unbounded enumerations
renderListGen :: NodeId -> DNode -> DNode -> Maybe DNode -> Int -> (String, String)
renderListGen resID min step max par = (ass, mod) where
    ass = concat [
            printf "BoundedEnum_%i enum_%i(clock, node_%i_done, " resID resID resID,
            printf "node_%i, node_%i, " (nodeID min) (nodeID step),
            maybe  "8'hFF, " (printf "node_%i, " . nodeID) max,
            chopComma $ argEdge par (DArgument resID (DList UndefinedType) (InferredPar par)),
            ");\n"
        ]
    mod = unlines [
              printf "module BoundedEnum_%i(input clock, input ready," resID,
              indent [
                  "input signed [7:0] min,",
                  "input [7:0] step,",
                  "input signed [7:0] max,",
                  "",

                  "input req,",
                  "output reg ack,",
                  unlines . parEdge par $ printf "output reg signed [7:0] value_%i,",
                  chopComma . unlines . parEdge par $ printf "output value_%i_valid,",
                  ");",
                  "",

                  "reg lastReq;",
                  "reg initialized, active;",
                  unlines . parEdge par $ printf "wire signed [7:0] nextValue_%i;",
                  unlines . parEdge par $ \i -> printf "assign nextValue_%i = value_%i + 8'd%i * step;" i i par,
                  unlines . parEdge par $ printfAll "assign value_%i_valid = ready & active & value_%i >= min && value_%i <= max;",
                  "",

                  "always @(posedge clock) begin",
                  indent [
                      "lastReq <= req;",
                      "",

                      "if(ready) begin",
                      indent [
                          "if(req & ~lastReq) begin",
                          indent [
                              "if(initialized) begin",
                              indent [
                                  printf "if(value_%i_valid) begin" $ par - 1,
                                  indent . parEdge par $ printfAll "value_%i <= nextValue_%i;",
                                  "end else begin",
                                  "    active <= 1'b0;",
                                  "end",
                                  ""
                              ],
                              "end else begin",
                              indent [
                                  "initialized <= 1;",
                                  unlines . parEdge par $ printfAll "value_%i <= min + 8'd%i;"
                              ],
                              "end",
                              "",

                              "ack <= 1;",
                              ""
                          ],

                          "end else begin",
                          "    ack <= 0;",
                          "end",
                          ""
                      ],

                      --if not ready, reset/initialize variables
                      "end else begin",
                      "    ack <= 0;",
                      "    initialized <= 0;",
                      "    active <= 1'b1;",
                      indent . parEdge par $ printf "value_%i <= 8'hXX;",
                      "end"
                  ],
                  "end"
              ],
              "endmodule",
              ""
          ]

--Generates assign statements for ready/done signals for builtin functions.
genericDone :: NodeId -> [DNode] -> String
genericDone resID args = ass where
    ass = printf "assign node_%i_done = %s;\n" resID doneArgs
    doneArgs = if   length args == 0
               then "ready"
               else joinMap " & " (printf "node_%i_done" . nodeID) args

--Helper function for extracting the contents of VNodeDefs
concatNodes :: [VNodeDef] -> String
concatNodes ns = defs ++ "\n" ++ assigns where
    (defs, assigns) = foldl f ("", "") $ uniqV ns

    --lift uniq over VNodeDef
    uniqV :: [VNodeDef] -> [VNodeDef]
    uniqV = map snd . uniq . map (\v -> (vNodeId v, v))

    --Concatenate definitions and assignments
    f :: (String, String) -> VNodeDef -> (String, String)
    f (ds, as) node = (ds ++ vDef node, as ++ vAssign node)

--Converts a scalar Haskell type to a Verilog type (i.e. a bus)
scalarVType :: DType -> String
scalarVType (DUInt n) = printf "[%i:0]" (n - 1)
scalarVType (DSInt n) = printf "[%i:0]" (n - 1)
scalarVType DBool = ""                                      --1 bit is implied by a blank type
scalarVType UndefinedType = scalarVType $ DUInt 8           --default

--Filters out key-value pairs which re-use existing keys. Preserves order.
uniq :: Ord a => [(a, b)] -> [(a, b)]
uniq xs = reverse $ foldl f [] xs where
    f res (key, value)
        | key `elem` (map fst res) = res                                    --key already defined; do nothing
        | otherwise                = (key, value):res                       --key not defined; add definition

--Helper function for indenting blocks of code
indent :: [String] -> String
indent = rstrip . unlines . map f where
    f = intercalate "\n" . map ('\t':) . lines

indentN :: Int -> [String] -> String
indentN n = unlines . map f where
    f = intercalate "\n" . map (delim ++) . lines
    delim = take n (repeat '\t')

--like printf, but replaces all %i with a single value.
printfAll :: String -> Int -> String
printfAll f i = replace "%i" (show i) f

