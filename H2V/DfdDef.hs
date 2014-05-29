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
                dfdArgs :: [(NodeId, DType)],
                returnType :: DType,
                isSync :: Bool,
                dfdRoot :: DNode
            }
            | DfdHeader{                            --Used as a placeholder during generation
                dfdID :: NodeId,
                dfdName ::String
            }
    deriving (Show, Eq)

data DNode = DLiteral{
                nodeID :: NodeId,
                literalValue :: Int
            }
            | DVariable{
                nodeID :: NodeId,
                variableType :: DType,
                variableValue :: Maybe DNode
            }
            | DBuiltin{
                nodeID :: NodeId,
                builtinOp :: BuiltinOp
            }
            | DFunctionCall{
                nodeID :: NodeId,
                functionCalled :: DFD,
                callArgs :: [DNode]
            }
    deriving (Show, Eq)

data BuiltinOp = BitwiseNot | BinaryOp String | Ternary
    deriving (Show, Eq)

-- supported data types: D_Int width. (May add fixed point support in the future)
-- Note that Haskell types for signed and unsigned integers are Int32 and Word32
data DType = DSInt Int | DUInt Int | DBool | UndefinedType
    deriving (Show, Eq)

isBuiltin :: DNode -> Bool
isBuiltin (DBuiltin _ _) = True
isBuiltin _ = False

isHeader :: DFD -> Bool
isHeader (DfdHeader _ _) = True
isHeader _ = False

isFunctionCall :: DNode -> Bool
isFunctionCall (DFunctionCall _ _ _) = True
isFunctionCall _ = False

--Returns a list of values which the node depends on. This does not include functions.
nodeChildren :: DNode -> [DNode]
nodeChildren DVariable{variableValue = Just v} = [v]
nodeChildren DFunctionCall{callArgs = a} = a
nodeChildren _ = []

--Convenience function for determining the type of a variable.
nodeType :: DNode -> DType
nodeType DVariable{variableType = t} = t
nodeType DFunctionCall{functionCalled = f} = returnType f

--Simplifies mapping over the DFD. Uses depth-first traversal. Does not pass through function calls.
--Does not check for infinite loops, since DFDs are trees.
dmap :: (DNode -> DNode) -> DNode -> DNode
dmap f n@(DVariable _ _ val) = f $ n{variableValue = liftM (dmap f) val}
dmap f n@(DFunctionCall _ _ args) = f $ n{callArgs = map (dmap f) args}
dmap f x = f x

dmapM :: Monad m => (DNode -> m DNode) -> DNode -> m DNode
dmapM f n@(DVariable _ _ val) = do
    val' <- if isJust val
           then (liftM Just) . (dmapM f) $ fromJust val
           else return val
    f $ n{variableValue = val'}

dmapM f n@(DFunctionCall _ _ args) = do
    args' <- mapM (dmapM f) args
    f n{callArgs = args'}

dmapM f x = f x

dfold :: (a -> DNode -> a) -> a -> DNode -> a
dfold f x0 n@(DVariable _ _ (Just val)) = dfold f (f x0 n) val
dfold f x0 n@(DFunctionCall _ _ args) = foldl (dfold f) (f x0 n) args
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

popNS :: Either (String, DNode) (String, DFD) -> NodeGen ()
popNS (Left n) = popNodeNS n
popNS (Right n) = popDfdNS n

resolveNode :: String -> NodeGen DNode
resolveNode name = do
    ns <- liftM nodeNS $ get
    return $ case filter (\(n, _) -> n == name) ns of
        (_, x):_ -> x
        [] -> throw $ ResolutionException "node" name (show $ map fst ns)

resolveDFD :: String -> NodeGen DFD
resolveDFD name = do
    ns <- liftM funcNS $ get
    return $ case filter (\(n, _) -> n == name) ns of
              (_, x):_ -> x
              [] -> throw $ ResolutionException "DFD" name (unlines $ map f ns) where
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
        let f (_, func) = isHeader func && dfdID func == fID

        return $ case filter f ns of
                  (_, x):_ -> x
                  [] -> throw $ ResolutionException "DFD" (printf "%s (id=%i)" fName fID) (unlines $ map f ns) where
                    f (name, dfd) = printf "\t[%2i] %s %s" (dfdID dfd) name (if isHeader dfd
                                                                             then "(header)"
                                                                             else "")

