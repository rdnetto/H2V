module DfdDef where

import Control.Monad.State
import Control.Exception (Exception, throw)
import Data.Maybe
import Data.Typeable
import Text.Printf

import Common

--DFD Types

{-
 - This module contains definitions of the DFD types and associated monads.
 -
 - A DFD is a graph, where nodes repesent values/computations, which are connected by edges.
 - Each function will be mapped to a DFD, and may be combinatorial or synchronous.
 - A DFD will be synchronous if its function is recursive or calls a function which is recursive.
 - Each DFD will have a name for descriptive purposes (may also store source location for debugging).
 -
 - A node may be one of the following types:
 - * literal constant
 - * argument (value sourced from external scope)
 - * built-in computation - provided by an operator in Verilog
 - * function call - the computation is performed by another DFD
 -
 - One DFD will be generated for each function.
 - Note that nodes may hold numeric values or functions. This needs to be stored explicitly, to assist in rendering.
 -
 - During generation, functions/DFDs are referred to by numeric identifiers. This is because functions within the same namespace can
 - depend on each other regardless of the order they are in.
 -
 - All DFDs used by a program are stored in a DProgram record
 -}

--NOTE: replace 'type X = X blah' with newtype?
type DProgram = [DFD]                                       --allDFDs              TODO: add info for exported functions
type NodeId = Int                                           --Used to assign nodes and graphs unique names

data DFD = DFD{
                dfdID :: NodeId,                    --This is the ID of the *function* - it is distinct from the ID of the root node.
                dfdName :: String,
                dfdArgs_ :: [(NodeId, DType)],
                returnType_ :: DType,
                isSync :: Bool,
                dfdRoot :: DNode
            }
            | DfdHeader{                            --Used as a placeholder during generation. TypeInfo is args ++ return type.
                dfdID :: NodeId,
                dfdName :: String,
                dfdTypeInfo :: Maybe ([(NodeId, DType)], DType)
            }
    deriving (Show, Eq)

data DNode = DLiteral{
                nodeID :: NodeId,
                literalValue :: Int
            }
            | DListLiteral {
                nodeID :: NodeId,
                elements :: [DNode],
                parallelism :: Parallelism
            }
            | DTupleElem {
                nodeID :: NodeId,
                tupleIndex :: Int,
                tuple :: DNode
            }
            | DVariable{
                nodeID :: NodeId,
                variableType :: DType,
                variableValue :: DNode
            }
            | DArgument{
                nodeID :: NodeId,
                variableType :: DType,
                parallelism :: Parallelism
            }
            | DBuiltin{
                nodeID :: NodeId,
                builtinOp :: BuiltinOp
            }
            | DFunctionCall{
                nodeID :: NodeId,
                functionCalled :: DFD,
                callArgs :: [DNode],
                parallelism :: Parallelism
            }
            | DFunction{
                nodeID :: NodeId,
                functionCalled :: DFD
            }
    deriving (Show, Eq)

data BuiltinOp = BitwiseNot | BinaryOp String | Ternary | EnumList | Decons | ListMinAvail | MapMacro | FoldMacro | ZipMacro
    deriving (Show, Eq)

-- supported data types: D_Int width. (May add fixed point support in the future)
-- Note that Haskell types for signed and unsigned integers are Int32 and Word32
data DType = DSInt Int | DUInt Int | DBool | UndefinedType
            | DList DType
            | DTuple [DType]
            | DFunc{
                funcArgs :: [DType],
                funcRT :: DType
            }
    deriving (Show, Eq)

--Describes parallelism assignments used for lists.
--Inferred values may be overridden to be compatible with assigned values.
data Parallelism = AssignedPar Int | InferredPar Int | NoPar
    deriving (Show, Eq)

isBuiltin :: DNode -> Bool
isBuiltin (DBuiltin _ _) = True
isBuiltin _ = False

isHeader :: DFD -> Bool
isHeader (DfdHeader{}) = True
isHeader _ = False

getOperator :: BuiltinOp -> String
getOperator (BinaryOp op) = op

isFunctionCall :: DNode -> Bool
isFunctionCall (DFunctionCall{}) = True
isFunctionCall _ = False

isBuiltinMacro :: DNode -> Bool
isBuiltinMacro db@(DBuiltin _ op)
    | op `elem` [MapMacro, FoldMacro, ZipMacro] = True
isBuiltinMacro _ = False

isHigherOrderFunc :: DFD -> Bool
isHigherOrderFunc f = any isFunc $ ret:(map snd args) where
    args = dfdArgs f
    ret = returnType f

isList :: DType -> Bool
isList (DList t) = True
isList _ = False

dfdArgs :: DFD -> [(NodeId, DType)]
dfdArgs dfd@DFD{} = dfdArgs_ dfd
dfdArgs DfdHeader{dfdTypeInfo = Just (args, ret)} = args

--Retrieves the actual nodes used for the DFD's args
trueArgs :: DFD -> [DNode]
trueArgs dfd = map getArg $ dfdArgs dfd where
    argNodes = map (\n -> (nodeID n, n)) . filterNodes isArg $ dfdRoot dfd

    getArg :: (NodeId, DType) -> DNode
    getArg (nID, t) = maybe (DArgument nID t NoPar) id $ lookup nID argNodes

returnType :: DFD -> DType
returnType dfd@DFD{} = returnType_ dfd
returnType DfdHeader{dfdTypeInfo = Just (args, ret)} = ret

isIf :: DNode -> Bool
isIf (DFunctionCall _ f _ _) = dfdID f == -1 && dfdName f == "if"
isIf _ = False

--Returns a list of values which the node depends on. This does not include functions.
nodeChildren :: DNode -> [DNode]
nodeChildren DVariable{variableValue = v} = [v]
nodeChildren DFunctionCall{callArgs = a} = a
nodeChildren _ = []

--Convenience function for determining the type of a variable.
nodeType :: DNode -> DType
nodeType DVariable{variableType = t} = t
nodeType DArgument{variableType = t} = t
nodeType DFunction{functionCalled = f} = DFunc (map snd $ dfdArgs f) (returnType f)
nodeType DFunctionCall{functionCalled = f} = returnType f
nodeType DLiteral{} = UndefinedType
nodeType DListLiteral{elements = x0:_} = DList $ nodeType x0
nodeType DListLiteral{} = DList UndefinedType
nodeType DTupleElem{tuple = t, tupleIndex = i}
    | nodeType t == UndefinedType = UndefinedType
    | otherwise                   = let DTuple ts = nodeType t
                                    in  ts !! i

--Selects the first type in the list that is not UndefinedType, or UndefinedType if there are no other elements available.
selectType :: [DType] -> DType
selectType (UndefinedType:ts) = selectType ts
selectType (t0:ts) = t0
selectType [] = UndefinedType

isArg :: DNode -> Bool
isArg DArgument{} = True
isArg _ = False

--Returns all nodes in the DFD sub-graph satisfied by the predicate
filterNodes :: (DNode -> Bool) -> DNode -> [DNode]
filterNodes pred root = dfold f [] root where
    f xs node
        | pred node = node:xs
        | otherwise = xs

isFunc :: DType -> Bool
isFunc DFunc{} = True
isFunc _ = False

--Returns the numeric degree of parallelism
parValue :: Parallelism -> Int
parValue (InferredPar p) = p
parValue (AssignedPar p) = p
parValue NoPar = 1

--Returns true if the node is capable of parallelism
hasParallelism :: DNode -> Bool
hasParallelism DFunctionCall{functionCalled = f}
    | any isList $ (returnType f):(map snd $ dfdArgs f) = True
hasParallelism DListLiteral{} = True
hasParallelism DArgument{} = True
hasParallelism _ = False

safeParallelism :: DNode -> Parallelism
safeParallelism n
    | hasParallelism n = parallelism n
    | otherwise        = NoPar

isAssigned :: Parallelism -> Bool
isAssigned (AssignedPar _) = True
isAssigned _ = False

--Simplifies mapping over the DFD. Uses depth-first traversal. Does not pass through function calls or DFunctions.
--Does not check for infinite loops, since DFDs are trees.
dmap :: (DNode -> DNode) -> DNode -> DNode
dmap f n@(DVariable _ _ val) = f $ n{variableValue = dmap f val}
dmap f n@(DTupleElem _ _ val) = f $ n{tuple = dmap f val}
dmap f n@(DFunctionCall _ _ args _) = f $ n{callArgs = map (dmap f) args}
dmap f x = f x

dmapM :: Monad m => (DNode -> m DNode) -> DNode -> m DNode
dmapM f n@(DVariable _ _ val) = do
    val' <- dmapM f val
    f $ n{variableValue = val'}

dmapM f n@(DTupleElem _ _ val) = do
    val' <- f val
    f n{tuple = val'}

dmapM f n@(DFunctionCall _ _ args _) = do
    args' <- mapM (dmapM f) args
    f n{callArgs = args'}

dmapM f x = f x

dfold :: (a -> DNode -> a) -> a -> DNode -> a
dfold f x0 n@(DVariable _ _ val) = dfold f (f x0 n) val
dfold f x0 n@(DTupleElem _ _ val) = dfold f (f x0 n) val
dfold f x0 n@(DFunctionCall _ _ args _) = foldl (dfold f) (f x0 n) args
dfold f x0 n = f x0 n

--DFD Generation Monad

{-
 - Monad for node generation. This stores information needed to generate unique IDs and perform scoped identifier resolution.
 - State monads take the form 'State stateType returnType' - we are currying the second argument.
 - UIDs are generated using a simple counter.
 - Identifier resolution is implemented as a hierarchial association list. The top-most tuple has precedence.
 - Note that nodes and functions/DFDs have separate resolution namespaces.
 -}
type NodeGen = State NodeGenData
data NodeGenData = NodeGenData {
    nextID   :: Int,
    nodeNS   :: [(String, DNode)],
    funcNS   :: [(String, DFD)],
    funcList :: [DFD]
}

initialNodeData :: NodeGenData
initialNodeData = NodeGenData 0 [] [] []

--modifier functions for mapping over a single field
modifyNextID f   = modify $ \ngd -> ngd { nextID = f $ nextID ngd }
modifyNodeNS f   = modify $ \ngd -> ngd { nodeNS = f $ nodeNS ngd }
modifyFuncNS f   = modify $ \ngd -> ngd { funcNS = f $ funcNS ngd }
modifyFuncList f = modify $ \ngd -> ngd { funcList = f $ funcList ngd }

data ResolutionException = ResolutionException String String String                      --scope name ns
    deriving (Typeable)
instance Exception ResolutionException
instance Show ResolutionException where
    show (ResolutionException scope name ns) = printf "Unable to resolve %s %s. Namespace:\n%s" scope name ns

unwrapEither :: (Either ResolutionException a) -> a
unwrapEither (Left e) = throw e
unwrapEither (Right x) = x

displayFunc :: DFD -> String
displayFunc f = printf "[%2i] %s %s" (dfdID f) (dfdName f) (if isHeader f then "(header)" else "")

--retrieve value expected to be known at compile-time
getConstant :: DNode -> Int
getConstant DLiteral{literalValue = x} = x
getConstant DVariable{variableValue = x} = getConstant x

--Monadic functions

--assigns a unique ID to the current node/DFD, incrementing the internal counter.
--all monadic functions have a return type of (State ...). They do not need to take a monadic argument. (Alternatively, this could be understood as monadic functions having (State ...) as the type of the last argument.)
newId :: NodeGen Int
newId = do
    oldID <- liftM nextID $ get
    modifyNextID succ
    return oldID

--namespace management functions. Pop takes the value expected to be popped as a sanity check.
pushNodeNS :: (String, DNode) -> NodeGen ()
pushNodeNS entry = modifyNodeNS (entry:)

pushDfdNS :: (String, DFD) -> NodeGen ()
pushDfdNS entry = modifyFuncNS (entry:)

pushNS :: Either (String, DNode) (String, DFD) -> NodeGen ()
pushNS (Left n) = pushNodeNS n
pushNS (Right n) = pushDfdNS n

popNodeNS :: (String, DNode) -> NodeGen ()
popNodeNS entry = do
    n0:ns <- liftM nodeNS $ get
    if entry == n0 then
        modifyNodeNS $ \_ -> ns
    else
        error $ printf "Error popping node NS.\nExpected: %s\nFound: %s" (fst entry) (show . map fst $ n0:ns)

popDfdNS :: (String, DFD) -> NodeGen ()
popDfdNS entry = do
    n0:ns <- liftM funcNS $ get
    if entry == n0 then
        modifyFuncNS $ \_ -> ns
    else
        error $ printf "Error popping DFD NS.\nExpected: %s\nFound: %s" (fst entry) (show . map fst $ n0:ns)

popNS :: ([(String, DNode)], Maybe (String, DFD)) -> NodeGen ()
popNS (ns, Just x) = mapM popNodeNS (reverse ns) >> popDfdNS x
popNS (ns, Nothing) = mapM popNodeNS (reverse ns) >> return ()

--resolve a token, which could be a variable or a function
resolve :: String -> NodeGen DNode
resolve name = do
    nodeCase <- resolveNode_ name
    funcCase <- resolveDFD_ name

    case (nodeCase, funcCase) of
        (Right n, _) -> return n
        (_, Right f) -> newId >>= \i -> return $ DFunction i f
        (Left (ResolutionException _ n1 n2), _) -> throw $ ResolutionException "node or DFD" n1 n2

resolveNode :: String -> NodeGen DNode
resolveNode n = resolveNode_ n >>= (\x -> return $ unwrapEither x)

resolveDFD :: String -> NodeGen DFD
resolveDFD n = resolveDFD_ n >>= (\x -> return $ unwrapEither x)

resolveNode_ :: String -> NodeGen (Either ResolutionException DNode)
resolveNode_ name = do
    ns <- liftM nodeNS $ get
    return $ case filter (\(n, _) -> n == name) ns of
        (_, x):_ -> Right x
        [] -> Left $ ResolutionException "node" name (show $ map fst ns)

resolveDFD_ :: String -> NodeGen (Either ResolutionException DFD)
resolveDFD_ name = do
    ns <- liftM funcNS $ get
    return $ case filter (\(n, _) -> n == name) ns of
              (_, x):_ -> Right x
              [] -> Left $ ResolutionException "DFD" name (unlines $ map f ns) where
                f (name, dfd) = printf "\t%s %s" name (if isHeader dfd
                                                       then "(header)"
                                                       else "")

resolveHeader :: DFD -> NodeGen DFD
resolveHeader header
    | dfdID header == -1 = return header
    | otherwise          = do
        let fID = dfdID header
        let fName = dfdName header
        ns <- liftM funcNS $ get

        --Return the resolved function, or if the original if it is fully defined.
        --We give the resolved function precedence since it could have been updated (e.g. closure rewriting).
        --If the function is not in scope but is fully defined, we can safely assume it has already been updated.
        return $ case filter (\f -> (dfdID . snd) f == fID) ns of
                  (_, x):_ -> x
                  [] -> if isHeader header
                        then throw $ ResolutionException "DFD" (displayFunc header) (unlines $ map (('\t':) . displayFunc . snd) ns)
                        else header


