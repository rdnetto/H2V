{-
 - Scoped type annotations are immensely helpful in debugging type inference problems with monads.
 - For example, we can annotate the result from (get :: MonadState) as being of type NodeGenData.
 - This is necessary to get comprehensible error messages.
 -}
{-# LANGUAGE ScopedTypeVariables #-}

module DfdDef where

import Control.Monad.State
import Text.Printf

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
data DFD = DFD NodeId String DType Bool DNode               --id name, returnType, isSync, root
data DNode = DLiteral NodeId Int                            --id value             TODO: include type?
            | DArgument NodeId DType                        --id type              TODO: extend this to support functional arguments
            | DBuiltin NodeId BuiltinOp DType [DNode]       --id op type args
            | DFunctionCall NodeId DFD [DNode]              --id function args
            | DFunctionCall_unresolved String [DNode]       --functionName args. (Only used duration generation)
data BuiltinOp = BitwiseNot | BinaryOp String | Ternary

-- supported data types: D_Int width. (May add fixed point support in the future)
-- Note that Haskell types for signed and unsigned integers are Int32 and Word32
data DType = DSInt Int | DUInt Int

instance Eq DNode
instance Eq DFD

instance Show DFD where
    show (DFD id name returnType isSync root) =
        printf "{DFD id=%i, name=%s, returnType=%s, isSync=%s, root=%s}" id name (show returnType) (show isSync) (show root)

instance Show DNode where
    show (DLiteral id x) = show ("DLiteral", id, x)
    show _ = error "failed pattern match"

instance Show DType where
    show (DUInt x) = "DUInt " ++ show x
    show (DSInt x) = "DSInt " ++ show x

--utility function to simplify mapping over a 3-tuple
fmap3 :: (a -> a, b -> b, c -> c) -> (a, b, c) -> (a, b, c)
fmap3 (f1, f2, f3) (x1, x2, x3) = (f1 x1, f2 x2, f3 x3)


{-
 - Monad for node generation. This stores information needed to generate unique IDs and perform scoped identifier resolution.
 - State monads take the form 'State stateType returnType' - we are currying the second argument.
 - UIDs are generated using a simple counter.
 - Identifier resolution is implemented as a hierarchial list of association lists. The top-most list has precedence.
 - Note that nodes and functions/DFDs have separate resolution namespaces.
 -}
type NodeGenData = (Int, [[(String, DNode)]], [[(String, DFD)]])
type NodeGen = State NodeGenData

initialNodeData :: NodeGenData
initialNodeData = (0, [], [])

--assigns a unique ID to the current node/DFD, incrementing the internal counter.
--all monadic functions have a return type of (State ...). They do not need to take a monadic argument. (Alternatively, this could be understood as monadic functions having (State ...) as the type of the last argument.)
newId :: NodeGen Int
newId = do
    (oldID, _, _) <- get
--    modify $ fmap3 (succ, id, id)
    return oldID

--namespace management functions. Pop takes the value expected to be popped as a sanity check.
pushNodeNS :: [(String, DNode)] -> NodeGen ()
pushNodeNS entry = modify $ fmap3 (id, (entry:), id)
pushDfdNS :: [(String, DFD)] -> NodeGen ()
pushDfdNS entry = modify $ fmap3 (id, id, (entry:))

popNodeNS :: [(String, DNode)] -> NodeGen ()
popNodeNS entry = do
    (_, n0:ns, _) <- get
    if entry == n0 then
    --    modify $ fmap3 (id, \_ -> ns, id)
        return ()
    else
        error $ printf "Error popping node NS.\nExpected: %s\nFound: %s" (show entry) (show n0)

popDfdNS :: [(String, DFD)] -> NodeGen ()
popDfdNS entry = do
    (_, _, n0:ns) <- get
    if entry == n0 then
    --    modify $ fmap3 (id, id, \_ -> ns)
        return ()
    else
        error $ printf "Error popping DFD NS.\nExpected: %s\nFound: %s" (show entry) (show n0)

resolveNode :: String -> NodeGen DNode
resolveNode name = do
    (_, ns, _) :: NodeGenData <- get
    return $ case concatMap (filter (\(n, _) -> n == name)) ns of
        (_, x):_ -> x
        [] -> error $ printf "Unable to resolve node '%s'. Namespace:\n%s" name (show ns)

resolveDFD :: String -> NodeGen DFD
resolveDFD name = do
    (_, _, ns) :: NodeGenData <- get
    return $ case concatMap (filter (\(n, _) -> n == name)) ns of
              (_, x):_ -> x
              [] -> error $ printf "Unable to resolve function '%s'. Namespace:\n%s" name (show ns)

