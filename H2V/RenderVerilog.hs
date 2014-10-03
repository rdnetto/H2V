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
    eCalls' (DFunctionCall _ fc _) = (dfdID fc == dfdID target)
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
extractListConds (Right (DFunctionCall _ DFD{dfdRoot = root} [eCount, list]))
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
                                           concatMap (renderArg "input" "node" True ",") (zip [0..] args),
                                           rstrip . chopComma $ renderArg "output" "node" True ", " (0, (nodeID root, retType)),
                                       ");"
                                       ],
                                       indent $ map (\(i, _) -> printf "wire node_%i_done; //arg" i) args,
                                       indent . lines $ concatNodes defs',
                                       indent $ map (\(i, _) -> printf "assign node_%i_done = ready;" i) args,
                                       '\t' : doneAssign,
                                       "endmodule\n"
                                      ]
    where
        defs = renderNode root
        retType = selectType [returnType_ dfd, nodeType root]
        doneAssign = printf "assign done = node_%i_done;" $ nodeID root
        --need to filter out definitions from the root node that are already present in the module def
        argDefs = map strip . lines $ concat [ concatMap (renderArg "" "node" True "\n") (zip [0..] args),
                                              renderArg "" "node" True "\n" (0, (nodeID root, retType))
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

--WIP: implement
renderRecursiveListFunc :: DFD -> [RecursiveCase] -> String
renderRecursiveListFunc dfd@(DFD dfdID name args _ _ root) recCases = ""

--Helper method for removing latches
nullArg :: Int -> String
nullArg i  = printf "outArg_%i = 8'hXX;\n" i

--Defines the wires for a node, but doesn't connect them to anything
defineNode :: NodeId -> DType -> String
defineNode nodeID t = defineNodeX (printf "node_%i" nodeID) t

defineNodeX :: String -> DType -> String
defineNodeX label (DList t) = res where
    res = unlines [ printf "wire %s_req, %s_ack;" label label,
                    printf "wire %s %s_value;" (scalarVType t) label,
                    printf "wire %s_value_valid;" label,
                    printf "wire %s_done;" label
                  ]

defineNodeX label t = unlines [line1, line2] where
    line1 = printf "wire %s %s;" (scalarVType t) label
    line2 = printf "wire %s_done;" label

--Generates the assign statements needed to connect two nodes. LHS is set to RHS.
assignNode :: DNode -> DNode -> String
assignNode lhs rhs
    | isList (nodeType lhs) || isList (nodeType rhs) = unlines res
    where
        lhsID = nodeID lhs
        rhsID = nodeID rhs
        res = [ printf "assign node_%i_req = node_%i_req;" rhsID lhsID,
                printf "assign node_%i_ack = node_%i_ack;" lhsID rhsID,
                printf "assign node_%i_value = node_%i_value;" lhsID rhsID,
                printf "assign node_%i_value_valid = node_%i_value_valid;" lhsID rhsID,
                printf "assign node_%i_done = node_%i_done;" lhsID rhsID
              ]

assignNode lhs rhs = unlines [line1, line2] where
    line1 = printf "assign node_%i = node_%i;" (nodeID lhs) (nodeID rhs)
    line2 = printf "assign node_%i_done = node_%i_done;" (nodeID lhs) (nodeID rhs)

--Defines an argument to a Verilog module.
--  io: the storage class. e.g. "input"/"output". Type will be omitted if storage class is blank.
--  prefix. e.g. "node".
--  useNodeId: whether the numeric identifier used will be the node ID or the argument index
--  (i, (ai, t): i is the arg index, ai is the node ID, t is the type
--  tail: a string to append to the end of the result. Useful for semicolons, etc.
renderArg :: String -> String -> Bool -> String -> (Int, (NodeId, DType)) -> String
renderArg io prefix useNodeId tail (i, (argID, DList t)) = concat lines where
    lines = [ printf "%s %s_%i_req%s"         invIo    prefix index tail,
              printf "%s %s_%i_ack%s"         io prefix index tail,
              printf "%s %s %s_%i_value%s"    io hwType prefix index tail,
              printf "%s %s_%i_value_valid%s" io prefix index tail
            ]
    hwType = if io == ""
             then ""
             else scalarVType t
    index = if useNodeId
            then argID
            else i
    invIo = maybe io id (lookup io [("input", "output"), ("output", "input")])

renderArg io prefix useNodeId tail (i, (argID, t)) = printf "%s %s %s_%i%s" io hwType prefix index tail where
    hwType = if io == ""
             then ""
             else scalarVType t
    index = if useNodeId
            then argID
            else i

renderNode :: DNode -> [VNodeDef]
renderNode (DLiteral nodeID value) = return $ VNodeDef nodeID def ass "" where
    def = defineNode nodeID UndefinedType
    ass = unlines [ printf "assign node_%i = %i;" nodeID value,
                    printf "assign node_%i_done = ready;" nodeID
                  ]

--arguments which are lists are already defined
renderNode (DVariable nodeID (DList _) Nothing) = []

--argument - wire/reg is defined with function, but need to manually specify ready/done signals
renderNode (DVariable nodeID _ Nothing) = return $ VNodeDef nodeID def ass "" where
    def = printf "wire node_%i_done;\n" nodeID
    ass = printf "assign node_%i_done = ready;\n" nodeID

renderNode var@(DVariable varID t (Just val)) = valDef ++ return (VNodeDef varID def ass "") where
    valDef = renderNode val
    def = defineNode varID t
    ass = assignNode var val

renderNode (DFunctionCall appID f args)
    | dfdID f == (-1) = aDefs ++ return (renderBuiltin appID (builtinOp $ dfdRoot f) args)
    | otherwise       = aDefs ++ return (VNodeDef appID def ass "")
    where
        def = unlines [ defineNode appID (returnType f),
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
        aAsses = concatMap argEdge args'
        resAss = chopComma . argEdge $ DVariable appID (returnType f) Nothing

renderNode elem@(DTupleElem elemID tupleIndex tuple) = (renderNode tuple) ++ return (VNodeDef elemID def ass "") where
    tupleID = nodeID tuple
    def = defineNode elemID (nodeType elem)
    ass = unlines $ case tupleIndex of
                     0 -> [ printf "assign node_%i = node_head_%i;" elemID tupleID,
                            printf "assign node_%i_done = node_%i_done;" elemID tupleID
                          ]
                     1 -> [ printf "assign node_tail_%i_req = node_%i_req;" tupleID elemID,
                            printf "assign node_%i_ack = node_tail_%i_ack;" elemID tupleID,
                            printf "assign node_%i_value = node_tail_%i_value;" elemID tupleID,
                            printf "assign node_%i_value_valid = node_tail_%i_value_valid;" elemID tupleID,
                            printf "assign node_%i_done = node_%i_done;" elemID tupleID
                          ]
                     2 -> [ printf "assign node_%i = node_head_%i_valid;" elemID tupleID,
                            printf "assign node_%i_done = node_%i_done;" elemID tupleID
                          ]
                     _ -> error $ "Invalid tuple index: " ++ show tupleIndex

--List literals are handled by generating a module to implement the list interface
renderNode (DListLiteral listID items) = (VNodeDef listID def ass mod):elemDefs where
    def = unlines [ printfAll "wire node_%i_req, node_%i_ack;" listID,
                    printfAll "wire [7:0] node_%i_value;" listID,
                    printfAll "wire node_%i_value_valid;" listID,
                    printfAll "wire node_%i_done;" listID
                  ]
    ass = unlines  [printfAll "listLiteral_%i listLit_%i(clock, ready," listID,
                    chopComma $ indent [
                        unlines $ map argEdge' items,
                        argEdge (DVariable listID (DList UndefinedType) Nothing)
                    ],
                    ");",
                    genericDone listID items
                  ]
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
                        "output [7:0] value,",
                        "output value_valid"
                    ],
                    ");\n",
                    indent [
                        "reg lastReq;",
                        "",
                        "assign value = 8'hXX;",
                        "assign value_valid = 0;",
                        "",
                        "always @(posedge clock) begin",
                        "\tack <= ready & ~lastReq & req;",
                        "\tlastReq <= req;",
                        "end"
                        ],
                    "endmodule"
                ]

    mod' = unlines [printf "module listLiteral_%i(" listID,
                    indent [
                        "input clock,",
                        "input ready,",
                        concatMap (printfAll $ unlines [
                                "input [7:0] x_%i,",
                                "input x_%i_done,"
                                ]) elemIndices,
                        "input req,",
                        "output reg ack,",
                        "output reg [7:0] value,",
                        "output reg value_valid"
                    ],
                    ");\n",
                    indent [
                        "reg done;",
                        "reg dummy;",
                        "reg lastReq;",
                        "reg [7:0] index;",
                        "",

                        "always @(*) begin",
                        indent [
                            "case (index)",
                            indent [
                                concatMap elemCase elemIndices,
                                "default: begin",
                                "\tdone = 1;",
                                "\tvalue = 8'hFF;",
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
                                "ack <= 0;",
                                "value_valid <= 0;"
                            ],
                            "end else if(req & ~lastReq) begin",
                            indent [
                                printf "if(index < 8'd%i || index == 8'hFF) begin" $ last elemIndices,
                                "\tvalue_valid <= 1;",
                                "\tindex <= index + 1;",
                                "end else",
                                "\tvalue_valid <= 0;",
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
                    "endmodule"
                  ]

    elemCase :: Int -> String
    elemCase i = printfAll fmt i where
        fmt = unlines [
                "%i: begin",
                indent [
                    "done = x_%i_done;",
                    "value = x_%i;"
                ],
                "   end"
              ]

    --variant which adds return and done signals
    argEdge' :: DNode -> String
    argEdge' a = value ++ done where
        value = renderArg "" "node" True ", " (0, (nodeID a, nodeType a))
        done  = renderArg "" "node" True "_done," (0, (nodeID a, DBool))

renderNode n = error $ "Unable to render:\n" ++ (show n)

argEdge :: DNode -> String
argEdge n = argEdgeX "node" n

argEdgeX :: String -> DNode -> String
argEdgeX lbl a = renderArg "" lbl True ", " (0, (nodeID a, nodeType a))

renderBuiltin :: NodeId -> BuiltinOp -> [DNode] -> VNodeDef
renderBuiltin resID BitwiseNot args@(arg:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    def = defineNode resID (nodeType arg)
    ass = printf "assign node_%i = ~node_%i;\n" resID (nodeID arg)
    doneAs = genericDone resID args

renderBuiltin resID (BinaryOp ":") args@(a0:a1:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    def = defineNode resID (nodeType a1)
    ass = concat [
            printf "Cons cons_%i(clock, node_%i_done, node_%i, " resID resID a0ID,
            argEdge (DVariable  a1ID (DList UndefinedType) Nothing),
            argEdge (DVariable resID (DList UndefinedType) Nothing),
            ");\n"
        ]
    a0ID = nodeID a0
    a1ID = nodeID a1
    doneAs = genericDone resID args

renderBuiltin resID (BinaryOp "++") args@(a0:a1:[]) = VNodeDef resID def ass "" where
    def = defineNode resID (nodeType a0)
    a0ID = nodeID a0
    a1ID = nodeID a1
    ass = unlines [
            printf "Concat concat_%i(clock, node_%i_done," resID resID,
            argEdge (DVariable  a0ID (DList UndefinedType) Nothing),
            argEdge (DVariable  a1ID (DList UndefinedType) Nothing),
            argEdge (DVariable resID (DList UndefinedType) Nothing),
            ");",
            printf "assign node_%i_done = node_%i_done & node_%i_done;" resID a0ID a1ID
        ]

renderBuiltin resID (BinaryOp op) args@(a0:a1:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    def = defineNode resID (nodeType a0)
    ass = printf "assign node_%i = node_%i %s node_%i;\n" resID (nodeID a0) op (nodeID a1)
    doneAs = genericDone resID args

renderBuiltin resID Ternary args@(cond:tExp:fExp:[]) = VNodeDef resID def (ass ++ doneAs) "" where
        resType = headOr UndefinedType $ filter (/= UndefinedType) [nodeType tExp, nodeType fExp]
        def = defineNode resID (nodeType tExp)
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

renderBuiltin resID EnumList args@(min:step:max:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    --bounded
    def = defineNode resID (DList UndefinedType)
    ass = renderListGen resID min step (Just max)
    doneAs = genericDone resID [min, step, max]

renderBuiltin resID EnumList args@(min:step:[]) = VNodeDef resID def (ass ++ doneAs) "" where
    --unbounded
    def = defineNode resID (DList UndefinedType)
    ass = renderListGen resID min step Nothing
    doneAs = genericDone resID [min, step]

renderBuiltin resID Decons [list] = VNodeDef resID def ass "" where
    listID = nodeID list
    def = concat [ defineNodeX (printf "node_head_%i" resID) UndefinedType,
                   defineNodeX (printf "node_tail_%i" resID) (DList UndefinedType),
                   printf "wire node_head_%i_valid;\n" resID,
                   printf "wire node_%i_done;\n" resID
                 ]

    ass = concat [ printf   "Decons decons_%i(clock, node_%i_done, node_%i_done,\n\t" listID listID resID,
                   strip  $ argEdge  list,
                   "\n\t",
                   lstrip . argEdgeX "node_head" $ DVariable resID UndefinedType Nothing,
                   lstrip $ printf   "node_head_%i_valid,\n\t" resID,
                   lstrip . chopComma $ argEdgeX "node_tail" $ DVariable resID (DList UndefinedType) Nothing,
                   ");\n"
                 ]

renderBuiltin resID MapMacro [lambda, list] = VNodeDef resID def ass mod where
    listID = nodeID list
    listType = nodeType list
    f = functionCalled lambda
    fID = dfdID f
    def = defineNode resID listType
    ass = concat [
            printf "Map_%i map_%i(clock, node_%i_done, node_%i_done,\n\t" resID resID listID resID,
            lstrip $ argEdge list,
            "\n\t",
            lstrip . chopComma . argEdge $ DVariable resID listType Nothing,
            ");\n"
          ]
    inputType = scalarVType . snd . head $ dfdArgs f
    outputType = scalarVType $ returnType f
    mod = unlines [
              printf "module Map_%i(" resID,
              indent [
                  --referring to args as list_0 and list_1 for input and output lists, respectively
                  "input clock, input ready, input done,",
                  "output reg  listIn_req,",
                  "input       listIn_ack,",
                  printf "input %s listIn_value," inputType,
                  "input       listIn_value_valid,",

                  "input            listOut_req,",
                  "output reg       listOut_ack,",
                  printf "output reg %s listOut_value," outputType,
                  "output reg       listOut_value_valid",
                  ");",
                  "",

                  --waitingForInput: waiting for a new value from listIn
                  --processingValue: waiting for f(listIn_value) to complete
                  --endOfInput:      the input list has been exhausted
                  --consumerWaiting: the consumer of the output list is waiting for a value
                  --consumerServed:  the consumer of the output list has been given a value
                  "reg wasReady;",
                  "reg waitingForInput, processingValue, endOfInput;",
                  "reg consumerServed;",
                  "wire consumerWaiting, valueProcessed;",
                  printf "wire %s nextVal;" outputType,
                  printf "dfd_%i lambda(clock, processingValue, valueProcessed, listIn_value, nextVal);" fID,
                  "",
                  "assign consumerWaiting = listOut_req & ~consumerServed;",
                  "",

                  "always @(posedge clock) begin",
                  indent [
                      "wasReady <= ready;",
                      "",
                      "if(ready) begin",
                      indent [
                          "if(~wasReady) begin",
                          "\tlistIn_req <= 1;",
                          "\twaitingForInput <= 1'b1;",
                          "end",
                          "",
                          "if(waitingForInput & listIn_ack) begin",
                          indent [
                              "waitingForInput <= 1'b0;",
                              "",
                              "if(listIn_value_valid) begin",
                              "\tprocessingValue <= 1'b1;",
                              "end else begin",
                              "\tendOfInput <= 1'b1;",
                              "end"
                          ],
                          "end",
                          "",
                          "if((valueProcessed | endOfInput) & consumerWaiting) begin",
                          indent [
                              "listOut_value <= nextVal;",
                              "listOut_value_valid <= ~endOfInput;",
                              "listOut_ack <= 1'b1;",
                              "consumerServed <= 1'b1;",
                              "if(~endOfInput) begin",
                              indent [
                                  "if (listIn_req) begin",
                                  "\tlistIn_req <= 1'b0;",
                                  "\tprocessingValue <= 1'b0;",
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
                          "if(~listOut_req)",
                          "\tconsumerServed <= 1'b0;"
                      ],
                      "end else begin",
                      indent [
                          "waitingForInput <= 1'b0;",
                          "processingValue <= 1'b0;",
                          "endOfInput <= 1'b0;",
                          "consumerServed <= 1'b0;"
                      ],
                      "end"
                  ],
                  "end"
              ],
              "endmodule"
          ]

--Generates assign statement for bounded and unbounded enumerations
renderListGen :: NodeId -> DNode -> DNode -> Maybe DNode -> String
renderListGen resID min step max = res where
    res = concat [
            printf "BoundedEnum enum_%i(clock, node_%i_done, " resID resID,
            printf "node_%i, node_%i, " (nodeID min) (nodeID step),
            maybe  "8'hFF, " (printf "node_%i, " . nodeID) max,
            chopComma $ argEdge (DVariable resID (DList UndefinedType) Nothing),
            ");\n"
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

