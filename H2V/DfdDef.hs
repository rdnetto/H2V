module DfdDef where

import Control.Monad.State
import Control.Exception (Exception, throw)
import Data.Typeable
import Text.Printf

import Common

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

data DFD = DFD NodeId String [(NodeId, DType)] DType Bool DNode       --id, name, args, returnType, isSync, root.
            | DfdHeader NodeId String                       --id, name. Acts as a placeholder during generation.
    deriving (Show, Eq)                                     --Note that DFD's id is distinct from its root node.

data DNode = DLiteral NodeId Int                            --id, value             TODO: include type?
            | DVariable NodeId DType (Maybe DNode)          --id, type, value       TODO: extend this to support functional arguments
            | DBuiltin NodeId BuiltinOp                     --id, op
            | DFunctionCall NodeId DFD [DNode]              --id, function, args
    deriving (Show, Eq)

data BuiltinOp = BitwiseNot | BinaryOp String | Ternary
    deriving (Show, Eq)

-- supported data types: D_Int width. (May add fixed point support in the future)
-- Note that Haskell types for signed and unsigned integers are Int32 and Word32
data DType = DSInt Int | DUInt Int | UndefinedType
    deriving (Show, Eq)

nodeID :: DNode -> NodeId
nodeID (DLiteral x _) = x
nodeID (DVariable x _ _) = x
nodeID (DBuiltin x _) = x
nodeID (DFunctionCall x _ _) = x

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
}

initialNodeData :: NodeGenData
initialNodeData = NodeGenData 0 [] [] []

--modifier functions for mapping over a single field
modifyNextID f   = modify $ \ngd -> ngd { nextID = f $ nextID ngd }
modifyNodeNS f   = modify $ \ngd -> ngd { nodeNS = f $ nodeNS ngd }
modifyFuncNS f   = modify $ \ngd -> ngd { funcNS = f $ funcNS ngd }

data ResolutionException = ResolutionException String String String                      --scope name ns
    deriving (Typeable)
instance Exception ResolutionException
instance Show ResolutionException where
    show (ResolutionException scope name ns) = printf "Unable to resolve %s '%s'. Namespace:\n%s" scope name ns

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
                f (name, DFD _ _ _ _ _ _) = "\t" ++ name
                f (name, DfdHeader _ _) = "\t" ++ name ++ " (header)"

resolveIdDFD :: NodeId -> NodeGen DFD
resolveIdDFD id = do
    ns <- liftM funcNS $ get

    let f = f' where
        f' (_, DfdHeader id2 _) = id == id2
        f' _ = False

    return $ case filter f ns of
              (_, x):_ -> x
              [] -> throw $ ResolutionException "DFD" ("ID=" ++ show id) (show ns)

