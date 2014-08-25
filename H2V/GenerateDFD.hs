--This module contains code for converting ASTs to DFDs
module GenerateDFD (astToDfd) where

import Control.Monad.State
import Data.Int
import Data.Word
import Data.List
import Data.Either
import Data.Maybe
import Data.Graph
import Language.Haskell.Syntax
import Text.Printf

import Common
import DfdDef


--TODO: need to store exported function list here
--TODO: need to find a sane way to import Prelude here
astToDfd :: HsModule -> DProgram
astToDfd (HsModule _ _ exportSpec _ decls) = evalState m initialNodeData where
    m = do
        --import prelude (this will need to go in a function/file of its own at some point...)
        --using nodeID=-1 for built-in functions, since they'll be implemented in handwritten Verilog and won't need assigned IDs
        let f = f' where
            f' op = pushDfdNS (op, DFD (-1) op (take 2 $ repeat (-1, UndefinedType)) UndefinedType False $ DBuiltin (-1) (BinaryOp op)) in
            mapM f ["+", "-", "*", "/", "==", "<", ">", "<=", ">="]
        pushDfdNS ("if", DFD (-1) "if" ((-1, DBool):(-1, UndefinedType):(-1, UndefinedType):[]) UndefinedType False $ DBuiltin (-1) Ternary)

        --local functions
        --Before generating functions, populate namespace with their headers. This is needed for recursive functions.
        let decls' = matchDecls . sortDecls $ map cleanDecl decls
        headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'

        -- [(Maybe a, Maybe b)] -> ([Maybe a], [Maybe b]) -> ([a], [b])
        (nodes, dfds) <- liftM (map2 catMaybes catMaybes . splitTuple) . mapM defineDecl $ decls'

        --replace headers with completed functions
        res <- mapM linkDFD $ map snd dfds

        --cleanup
        mapM popDfdNS $ reverse dfds
        mapM popDfdNS $ reverse headers

        --collect functions
        concatMapM collectDfds res
        return <=< liftM funcList $ get

--cleaning logic

--rewrites expressions to remove irrelevant syntactic differences. e.g. infix vs prefix application
--the following white-listed expressions are the only ones we will support
cleanExpr :: HsExp -> HsExp
cleanExpr exp@(HsVar _) = exp
cleanExpr exp@(HsLit _) = exp
cleanExpr (HsLet decls exp) = HsLet (map cleanDecl decls) $ cleanExpr exp
cleanExpr (HsApp e1 e2) = HsApp (cleanExpr e1) (cleanExpr e2)
--replace IFs with a function call
--TODO: add support for qualified function names, so that we can avoid if being overloaded
cleanExpr (HsIf cond tExp fExp) = cleanExpr $ HsApp (HsApp (HsApp f cond) tExp) fExp where
    f = HsVar $ UnQual $ HsIdent "if"
--convert infix application to prefix application
cleanExpr (HsInfixApp arg1 op arg2) = case op of
                                        HsQVarOp opName -> newExpr opName
                                        HsQConOp opName -> newExpr opName
                                      where
    newExpr opName = HsApp (HsApp (HsVar opName) arg1') arg2'
    arg1' = cleanExpr arg1
    arg2' = cleanExpr arg2
--convert the unary negation operator to subtraction. e.g. -x => 0 - x => (-) 0 x
cleanExpr (HsNegApp exp) = cleanExpr $ HsInfixApp (HsLit $ HsInt $ 0) (HsQVarOp $ UnQual $ HsSymbol "-") (cleanExpr exp)
--remove parentheses, since they're redundant
cleanExpr (HsParen exp) = cleanExpr exp
--convert lambdas to regular functions, folding nested lambdas into a single one
cleanExpr (HsLambda s p0 (HsLambda _ p1 e1)) = cleanExpr $ HsLambda s (p0 ++ p1) e1
cleanExpr (HsLambda s p e) = cleanExpr $ HsLet [f] (HsVar . UnQual $ lambdaName) where
    lambdaName = HsIdent $ printf "lambda_%s_%i:%i" (srcFilename s) (srcLine s) (srcColumn s)
    f = HsFunBind [HsMatch s lambdaName p (HsUnGuardedRhs e) []]

cleanExpr exp = error $ "Unknown expression: " ++ pshow exp

--cleans declarations
cleanDecl :: HsDecl -> HsDecl
--TODO: Refactor: PatBind should use the same code as FunBind for pattern matching.
cleanDecl (HsPatBind src pat (HsUnGuardedRhs expr) decls) = HsPatBind src pat (HsUnGuardedRhs $ cleanExpr expr) (map cleanDecl decls)
cleanDecl (HsFunBind matches) = HsFunBind [res] where
    --using HsWildCard to represent non-exhaustive pattern matching
    --initial value m0 is used for the outermost match
    res = foldl cleanMatch m0 matches $ HsWildCard

    --using info from first match for the resultant match object
    --we use a generic set of variable names for args, since each match can given them different names (or none at all)
    (HsMatch src name pats _ _):_ = matches
    m0 = \expr -> HsMatch src name args (HsUnGuardedRhs expr) []
    args = take (length pats) $ map HsPVar $ genArgs
cleanDecl s@(HsTypeSig _ _ _) = s

cleanDecl d = error $ "Unknown declaration: " ++ pshow d

--Cleans function matches. Used in folding. leftM is the left (folded) match
--The result is a function of the expression returned if the pattern fails
--The left argument will have precedence over the right.
cleanMatch :: (HsExp -> HsMatch) -> HsMatch -> (HsExp -> HsMatch)
cleanMatch leftM (HsMatch _ _ pats rhs decls) = res where
    --we apply HsWildCard to the tail, so its presence means we assume that the last pattern matches
    res :: HsExp -> HsMatch
    res HsWildCard = leftM $ trueExp
    res elseExpr = leftM $ cleanExpr $ HsIf patternsMatch trueExp elseExpr

    patternsMatch = foldl1 andConds $ (trueExpr:) $ map patternMatches $ zip genArgs pats
    expr = case rhs of
        HsUnGuardedRhs e -> cleanExpr e
        HsGuardedRhss guards -> cleanExpr $ (foldr unguard tailGuardExp $ init guards) where
            HsGuardedRhs _ _ tailGuardExp = last guards             --TODO: add explicit check for assumption that last case if always true

    --guarded RHSs are basically nested IFs
    unguard :: HsGuardedRhs -> HsExp -> HsExp
    unguard (HsGuardedRhs _ con exp) elseExp = HsIf con exp elseExp

    --bind args, then subdecls
    trueExp = HsLet (concat $ zipWith bindPattern genArgs pats) $ HsLet (map cleanDecl decls) expr

    --given multiple Boolean expressions, AND them together
    andConds :: HsExp -> HsExp -> HsExp
    andConds left right
        | left == trueExpr  = right
        | right == trueExpr = left
        | otherwise         = HsInfixApp left (HsQVarOp $ UnQual $ HsIdent "&&") right

--yields a boolean expression which is true if the pattern is matched
--the argument is a 2-tuple specifying the new argument name and the pattern to be matched
patternMatches :: (HsName, HsPat) -> HsExp
patternMatches (_, HsPVar _) = trueExpr
patternMatches (_, HsPWildCard) = trueExpr
patternMatches (name, HsPNeg pat) = HsApp (HsVar $ UnQual $ HsIdent "not") $ patternMatches (name, pat)
patternMatches (name, HsPLit lit) = HsInfixApp (HsVar $ UnQual name) (HsQVarOp $ UnQual $ HsSymbol "==") (HsLit lit)
patternMatches (name, pat) = error $ printf "Unknown pattern in %s:\n%s" (show name) (show pat)

--Performs pattern binding. Note that each pattern can result in multiple bindings due to destructuring
bindPattern :: HsName -> HsPat -> [HsDecl]
bindPattern argName pat@(HsPVar _) = return $ HsPatBind (SrcLoc "" 0 0) pat rhs [] where
    rhs = HsUnGuardedRhs . HsVar . UnQual $ argName
bindPattern _ (HsPLit _) = []
bindPattern _ HsPWildCard = []
bindPattern _ p = error $ "Unknown declaration: " ++ pshow p

--DFD generation logic

createDfdHeaders :: (HsDecl, Maybe HsDecl) -> NodeGen (Maybe (String, DFD))
createDfdHeaders (HsFunBind [HsMatch _ name _ _ _], sig) = do
    rootID <- newId
    let name' = fromHsName name

    --convert from AST types to DTypes
    let extractTypes (HsTypeSig _ _ (HsQualType [] t)) = toDTypes t

    --assign each arg an ID
    let assignIDs t = newId >>= \i -> return (i, t)

    typeInfo <- ifM (return $ isJust sig)
        (do
            let types = extractTypes $ fromJust sig
            args <- mapM assignIDs $ init types
            return $ Just (args, last types)
        ) (return Nothing)

    let res = DfdHeader rootID name' typeInfo
    pushDfdNS (name', res)
    return $ Just (name', res)
--need to create a header for the results of higher order functions
createDfdHeaders (HsPatBind src pat@(HsPVar name) rhs decl, s) = createDfdHeaders (HsFunBind [HsMatch src name [pat] rhs decl], s)
createDfdHeaders (d, _) = error $ pshow d

toDTypes :: HsType -> [DType]
toDTypes f@(HsTyFun _ _) = unfold f where

    unfold :: HsType -> [DType]
    unfold (HsTyFun t1 t2) = (unfold t1) ++ [toDT t2]
    unfold t@(HsTyVar _) = return $ toDT t
    unfold t@(HsTyCon _) = return $ toDT t

    toDT :: HsType -> DType
    toDT (HsTyVar _) = UndefinedType
    toDT (HsTyCon _) = UndefinedType
    toDT f@(HsTyFun _ _) = DFunc (init ts) (last ts) where
        ts = unfold f

--defines a function argument. Similar to definePat, but without binding
--Populates the namespace immediately on creation, for consistency with defineDecl.
defineUntypedArg :: HsPat -> NodeGen (String, DNode, Maybe DFD)
defineUntypedArg p@(HsPVar name) = do
    nodeID <- newId
    defineTypedArg p (nodeID, UndefinedType)

defineTypedArg :: HsPat -> (NodeId, DType) -> NodeGen (String, DNode, Maybe DFD)
defineTypedArg (HsPVar name') (nodeID, DFunc args ret) = do
    --[arg] -> [(id, arg)]
    args' <- (flip mapM) args $ \a -> newId >>= \i -> return (i, a)
    let name = fromHsName name'
    let dfd = DfdHeader nodeID name $ Just (args', ret)
    let node = DFunction nodeID dfd
    pushNodeNS (name, node)
    pushDfdNS (name, dfd)
    return (name, node, Just dfd)
defineTypedArg (HsPVar name') (nodeID, t) = do
    let name = fromHsName name'
    let node = DVariable nodeID t Nothing
    pushNodeNS (name, node)
    return (name, node, Nothing)

--Generates a node for a variable bound to a pattern.
--This may need to be able to return multiple nodes, due to destructuring, etc.
--Note that bindPattern limits what we will see here - outputting multiple nodes might be unnecessary.
definePat :: HsPat -> DNode -> NodeGen (String, DNode)
definePat (HsPVar name) value = do
    nodeID <- newId
    return $ (fromHsName name, DVariable nodeID UndefinedType (Just value))

--Generates nodes/DFDs for declarations. These can be either variables/expressions (left case) or functions (right case).
--Returns namespace info.
--Declaration structure: lhs = rhs where subterms
--
--NOTE: This function pushes the declaration to the namespace stack immediately on creation, and it is the caller's responsibility to
--  pop them afterwards. This is necessary so that multiple declarations which refer to each other can be handled with mapM.
--
--TODO: this should be the top-level function called by astToDfd. This would centralize function gathering logic, and allow the use of global variables (via CAFs and patterns)
--We can neglect the signatures here because they are used to generate the headers.
defineDecl :: (HsDecl, Maybe HsDecl) -> NodeGen (Maybe (String, DNode), Maybe (String, DFD))
defineDecl (HsPatBind _ pat (HsUnGuardedRhs expr) decls, _) = do
    --subterms are automatically pushed on creation
    let decls' = matchDecls $ sortDecls decls
    headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
    terms <- mapM defineDecl $ decls'

    --define the RHS, link it, and bind it to the LHS
    rhs <- dmapM linkExpr <=< defineExpr $ expr
    lhs <- definePat pat rhs

    --cleanup subterms
    mapM popNS $ reverse terms
    mapM popDfdNS $ reverse headers

    --Push term, now that we've created it. This is necessary as other terms within the same list may refer to it.
    pushNodeNS lhs
    return $ (Just lhs, Nothing)

defineDecl (HsFunBind [HsMatch _ name pats (HsUnGuardedRhs expr) decls], _) = do
    --use the same ID as the header. This avoids issues related to shadowing.
    DfdHeader rootID _ typeInfo <- resolveFunc $ HsVar $ UnQual name

    --[(name, node, Maybe dfd)]
    args <- case typeInfo of
                Nothing           -> mapM defineUntypedArg pats
                Just (oldArgs, _) -> zipWithM defineTypedArg pats oldArgs
    let nodeArgs = (flip map) args $ \(name, node, _) -> (name, node)
    let dfdArgs  = catMaybes $ map dfdArg args where
        dfdArg (name, _, Just dfd) = Just (name, dfd)
        dfdArg (_, _, Nothing) = Nothing

    --headers are needed in case we have recursive functions
    let decls' = matchDecls $ sortDecls decls
    headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
    terms <- mapM defineDecl $ decls'

    --link functions. <=< is the monadic composition operator (analogous to .)
    root <- dmapM linkExpr <=< defineExpr $ expr

    mapM popNS $ reverse terms
    mapM popDfdNS $ reverse headers
    mapM popDfdNS $ reverse dfdArgs
    mapM popNodeNS $ reverse nodeArgs

    --id, name, returnType, isSync, root
    let name' = fromHsName name
    let args' = (flip map) nodeArgs $ (\x -> (x, UndefinedType)) . nodeID . snd

    let res = case typeInfo of
                Nothing             -> DFD rootID name' args' UndefinedType False root
                Just (oldArgs, ret) -> DFD rootID name' oldArgs ret False root

    pushDfdNS (name', res)
    return $ (Nothing, Just (name', res))

--generates/resolves nodes for expressions
defineExpr :: HsExp -> NodeGen DNode
defineExpr (HsVar (UnQual name)) = resolve $ fromHsName name
defineExpr (HsLit (HsInt val)) = do
    nodeID <- newId
    return $ DLiteral nodeID $ fromIntegral val

defineExpr (HsLet decls exp) = do
    let decls' = matchDecls $ sortDecls decls
    headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
    terms <- mapM defineDecl $ decls'                       --locals are pushed on creation

    root <- dmapM linkExpr <=< defineExpr $ exp
    mapM popNS $ reverse terms                              --cleanup locals
    mapM popDfdNS $ reverse headers
    return root

defineExpr app@(HsApp _ _) = do
    let (f, args) = foldApp app
    nodeID <- newId
    f' <- resolveFunc f
    args' <- mapM defineExpr args
    return $ DFunctionCall nodeID f' args'

defineExpr e = error $ "Failed to match expression: " ++ pshow e

--Combines repeated applications to collect a list of arguments
--Returns a 2-tuple of the function and its arguments.
--Note that the first application we encounter will be the outermost. i.e. the last argument. e.g f a b c = (((f a) b) c)
foldApp :: HsExp -> (HsExp, [HsExp])
foldApp (HsApp a0@(HsApp _ _) x) = res where
    (f, xs) = foldApp a0
    res = (f, xs ++ [x])                                     --x goes last, because we parse application from the outside-in
foldApp (HsApp f x) = (f, [x])

--Resolves an expression into a function (DFD).
--The expression may simply be the name of a known function, or it may be a lambda or curried expression.
resolveFunc :: HsExp -> NodeGen DFD
resolveFunc (HsVar name) = liftM funcCalled . resolve $ fromHsQName name where
    funcCalled f@DFunctionCall{} = functionCalled f
    funcCalled f@DFunction{} = functionCalled f
    funcCalled DVariable{variableValue = Just f} = functionCalled f
resolveFunc l@(HsLet _ _) = (liftM functionCalled) $ defineExpr l

--This function re-orders declarations so that their data dependencies are met.
--This is necessary because the order of the declarations does not affect resolution; a term defined later in the list can shadow
--something defined at a broader scope. Consequently the declarations must be reordered to ensure correct resolution.
--Note that while we handle shadowing correctly, we do not allow the same term to be defined multiple times, since that is only used in
--do-blocks which do not need to be sorted.
--
--NOTE: We are not allowing for mutual recursion here. While this could be a legitemate use case, it adds considerable complexity and
--      is rarely used.
sortDecls :: [HsDecl] -> [HsDecl]
sortDecls decls = res where
    --this function is called post-cleaning, so we shouldn't need to worry about patterns
    names = map declName decls
    deps = (flip map) decls $ filter (`elem` names) . declDeps
    z = if unique names
        then zip names deps
        else error $ "Duplicate identifiers found: " ++ show names

    --TODO: look at rewriting this to avoid O(N^2) behaviour, since N can be large
    g = buildG (-1, length z - 1) edges
    edges = concatMap f0 z where
        f0 (n, ds) = map (f1 n) ds                                              --maps a declaration to a set of edges
        f1 n dep = (idx n, idx dep)                                             --maps a dep to an edge
        idx n = fromMaybe (-1) $ elemIndex n names                              --maps a token to an index, or -1 if out of scope
    ds = filter (/= -1) . reverse $ topSort g
    res = map (decls !!) ds

    --these functions are for collecting a list of functions/variables which each declaration depends on
    declDeps :: HsDecl -> [String]
    declDeps (HsPatBind _ _ rhs _) = rhsDeps rhs
    declDeps (HsFunBind ms) = concatMap f ms where
        f (HsMatch _ _ _ rhs _) = rhsDeps rhs
    declDeps (HsTypeSig _ _ _) = []

    exprDeps :: HsExp -> [String]
    exprDeps (HsVar n) = return $ fromHsQName n
    exprDeps (HsLit _) = []
    exprDeps (HsApp a b) = exprDeps a ++ exprDeps b
    exprDeps (HsLet d e) = concatMap declDeps d ++ exprDeps e
    exprDeps (HsIf a b c) = exprDeps a ++ exprDeps b ++ exprDeps c

    rhsDeps :: HsRhs -> [String]
    rhsDeps (HsUnGuardedRhs e) = exprDeps e
    rhsDeps (HsGuardedRhss gs) = concatMap f gs where
        f (HsGuardedRhs _ e1 e2) = concatMap exprDeps [e1, e2]

--This function pairs type signatures to their functions
matchDecls :: [HsDecl] -> [(HsDecl, Maybe HsDecl)]
matchDecls decls = map findSig funcs where
    (sigs, funcs) = partition isTypeSig decls

    isTypeSig :: HsDecl -> Bool
    isTypeSig (HsTypeSig _ _ _) = True
    isTypeSig _ = False

    findSig :: HsDecl -> (HsDecl, Maybe HsDecl)
    findSig decl = (decl, sig) where
        sig = find (\s -> declName decl == declName s) sigs

declName :: HsDecl -> String
declName (HsFunBind ((HsMatch _ n _ _ _):_)) = fromHsName n
declName (HsPatBind _ (HsPVar n) _ _) = fromHsName n
declName (HsTypeSig _ [n] _) = fromHsName n

--linking logic - replaces function headers with DFDs
--FIX: now that we have access to function headers, we need to make sure foldApp wasn't too aggressive.
--e.g. (f a) b should be two separate applications if we have f :: Int -> (Int -> Int)

linkDFD :: DFD -> NodeGen DFD
linkDFD dfd = liftM (\x -> dfd{dfdRoot = x}) $ dmapM linkExpr (dfdRoot dfd)

linkExpr :: DNode -> NodeGen DNode
linkExpr fc@(DFunctionCall _ f _) = resolveHeader f >>= \f' -> checkArgs fc{functionCalled = f'}
linkExpr x = return x

--Check that the no. of args matches the function signature.
--This may not be the case if we've combined the applications of args to a higher order function and its result.
checkArgs :: DNode -> NodeGen DNode
checkArgs (DFunctionCall id f args)
    | length args == funcArgCount = return $ DFunctionCall id f args
    | length args <  funcArgCount = error $ printf "Incorrect no. of args.\nFunction: %s\nArgs: %s" (show f) (show args)
    | length args >  funcArgCount = do
        let f1 = DFunctionCall id f   (take funcArgCount args)             --higher order function - returns lambda
        f1' <- instantiateLambda f1
        let f2 = DFunctionCall id f1' (drop funcArgCount args)             --call to lambda
        return f2
    where
        funcArgCount = length $ dfdArgs f

--Defines the DFD of an instance of a lambda function.
--Clones nodes and substitutes args so that it is independent of other instances.
instantiateLambda :: DNode -> NodeGen DFD
instantiateLambda (DFunctionCall _ macro mArgs)
    | isHigherOrderFunc macro = do
        id' <- newId
        let f = functionCalled $ dfdRoot macro
        let macroArgs = zip (map fst $ dfdArgs macro) mArgs                --args from higher order func - substitute these for literals
        let oldArgs = map fst $ dfdArgs f                                  --args native to lambda - can't just clone them
        newArgs <- mapM cloneArg (dfdArgs f)                               --  since they're stored in the DFD as well
        let args' = map (\n -> (nodeID n, variableType n)) newArgs
        root' <- liftM (dmap (subArgs $ zip oldArgs newArgs)) . dmapM (cloneNodes oldArgs) . dmap (subArgs macroArgs) $ dfdRoot f
        return f{dfdID = id', dfdRoot = root', dfdArgs_ = args'}

--replace nodes
subArgs :: [(NodeId, DNode)] -> DNode -> DNode
subArgs dict node = maybe node id $ lookup (nodeID node) dict

--gives each node a new ID (except for those in the list)
cloneNodes :: [NodeId] -> DNode -> NodeGen DNode
cloneNodes whitelist node
    | (nodeID node) `elem` whitelist = return node
    | otherwise                      = newId >>= \n -> return node{nodeID = n}

cloneArg :: (NodeId, DType) -> NodeGen DNode
cloneArg (_, t) = do
    i <- newId
    return $ DVariable i t Nothing

--WIP: Closure logic - converts closures to function arguments

--Need to perform 2 passes - one to collect functions, and one to modify function calls
--The first pass will recurse into function definitions, so it needs to worry about infinite loops from recursion
type RewriteList = (NodeId, [(NodeId, NodeId)])             --DFD ID, association list of foreign node IDs and the corresponding args
data NodeLocality = Local | Mixed | Foreign deriving Eq

--First pass: collect functions, and rewrite definitions.
--Collected functions are stored in the monad, and used to avoid infinite loops due to recursion.
--Note that the DFDs stored in the function calls will be out of date until we use rewriteDfd
--Returns: an association list of rewritten DFD IDs and the arguments added to them.
collectDfds :: DFD -> NodeGen [RewriteList]
collectDfds func = ifM (funcCollected func) (return []) $ do
    --process current function
    let (func', (_, argAL)) = rewriteFuncDef func $ closedArgs func
    modifyFuncList (func':)

    --recurse into other functions
    let calls = filter isFunctionCall $ dfold (flip (:)) [] (dfdRoot func)
    rewrites <- concatMapM (collectDfds . functionCalled) calls

    return $ (dfdID func', argAL):rewrites

--second pass: rewrite function calls
closeOverDFD :: RewriteList -> DFD -> NodeGen DFD
closeOverDFD rewriteList f = return f

--Collects a list of all nodes that are external to the function.
--Note that what we are effectively doing is drawing a boundary/cut between the two functions' DFDs.
--Cuts which intersect with more edges imply that a greater no. of values must be passed between the functions.
--However, minimizing the no. of edges comes at the cost of duplicating logic.
--Since routing is (comparatively) cheap compared to delays, we use zero duplicate logic and maximal data transfer.
--In practical terms, this means returning every referenced node outside of our scope.
--
--Algorithm:
--We process the graph from the root up. We say that each node is local, foreign or mixed. Mixed nodes fall on the cut.
--Arguments (leaves) belonging to the function are local, and all other arguments are foreign.
--If all the children of a node are local/foreign, that node is local/foreign.
--If a node has local and foreign children, it is a mixed node.
--If a node has mixed chidren, it is a mixed node.
--The result is the set of all nodes which are foreign children of mixed nodes.
--In other words, the resultant set will consist of foreign nodes, and the paths to them will consist of mixed nodes. All other
--  nodes will be local.
closedArgs :: DFD -> [DNode]
closedArgs f = closedArgs' $ dfdRoot f where
    --returns all children which are the foreign child of a mixed node, including the root node if it is foreign.
    closedArgs' :: DNode -> [DNode]
    closedArgs' n = res where
        (loc, cnl) = nodeLocality n
        res = case loc of
            Local -> []
            Mixed -> concatMap (closedArgs' . fst) $ filter (\(c, cl) -> cl /= Local) cnl
            Foreign -> [n]

    --returns the locality of a node, and association list of localities for its children
    nodeLocality :: DNode -> (NodeLocality, [(DNode, NodeLocality)])
    nodeLocality n = (res, zip children childLocalities) where
        children = nodeChildren n
        childLocalities = map (fst . nodeLocality) children
        res' = foldl1 f1 childLocalities where
            f1 a b
                | a == b    = a
                | otherwise = Mixed                 --this works because if any child is mixed, the parent is also mixed

        res = if length children == 0
              then f2 n                             --WIP: f2 needs to determine if an arg is local or foreign. However,
              else res'                             --we need a list of the function's args to do this.

        f2 _ = Local

--Rewrites a function so that its closure is replaced by arguments
rewriteFuncDef :: DFD -> [DNode] -> (DFD, RewriteList)
rewriteFuncDef f _ = (f, (dfdID f, []))

--Checks if the function has been collected
funcCollected :: DFD -> NodeGen Bool
funcCollected DFD{dfdID = (-1)} = return True                                       --built-in functions do not need to be rendered
funcCollected func = do
    funcs <- liftM (map dfdID . funcList) $ get
    return $ dfdID func `elem` funcs

--utility functions

fromHsName :: HsName -> String
fromHsName (HsIdent x) = x
fromHsName (HsSymbol x) = x

fromHsQName :: HsQName -> String
fromHsQName (UnQual n) = fromHsName n
fromHsQName (Qual (Module m) n) = m ++ "." ++ fromHsName n

--yields an infinite list of arg names
genArgs :: [HsName]
genArgs = map (\a -> HsIdent $ "_arg_" ++ (show a)) [0..]

--TODO: this should return an expression equal to True
trueExpr :: HsExp
trueExpr = unit_con

--Returns False if any element appears the in the list more than once.
unique :: Ord a => [a] -> Bool
unique xs = or . (True:) . map f . zip xs' $ tail xs' where
    xs' = sort xs
    f (a, b) = a == b

--Lifts if-then-else over a monad. Useful for monadic conditions.
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond trueM falseM = cond >>= (\b -> if b then trueM else falseM)
