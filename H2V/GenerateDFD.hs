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
            f' op argN = pushDfdNS (op, DFD (-1) op (take argN $ repeat (-1, UndefinedType)) UndefinedType False $ DBuiltin (-1) (BinaryOp op)) in
            zipWithM f ["+", "-", "*", "/", "==", "if"] [2, 2, 2, 2, 2, 3]

        --local functions
        --Before generating functions, populate namespace with their headers. This is needed for recursive functions.
        let decls' = sortDecls $ map cleanDecl decls
        headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
        dfds <- (liftM rights) . mapM defineDecl $ decls'

        --replace headers with completed functions
        res <- mapM linkDFD $ map snd dfds

        --cleanup
        mapM popDfdNS $ reverse dfds
        mapM popDfdNS $ reverse headers
        return res

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
        _ -> error "Guarded RHSs are not implemented yet"               --Guarded RHS can have multiple expressions?
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

createDfdHeaders :: HsDecl -> NodeGen (Maybe (String, DFD))
createDfdHeaders (HsFunBind [HsMatch _ name _ _ _]) = do
    rootID <- newId
    let name' = fromHsName name
    let res = DfdHeader rootID name'
    pushDfdNS (name', res)
    return $ Just (name', res)
createDfdHeaders (HsPatBind _ _ _ _) = return Nothing                         --pattern bindings are CAFs, so they don't need headers
createDfdHeaders d = error $ pshow d

--defines a function argument. Similar to definePat, but without binding
--Populates the namespace immediately on creation, for consistency with defineDecl.
defineArg :: HsPat -> NodeGen (String, DNode)
defineArg (HsPVar name) = do
    nodeID <- newId
    let res = (fromHsName name, DVariable nodeID UndefinedType Nothing)
    pushNodeNS res
    return res

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
defineDecl :: HsDecl -> NodeGen (Either (String, DNode) (String, DFD))
defineDecl (HsPatBind _ pat (HsUnGuardedRhs expr) decls) = do
    --subterms are automatically pushed on creation
    let decls' = sortDecls decls
    headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
    terms <- mapM defineDecl $ decls'

    --define the RHS, link it, and bind it to the LHS
    rhs <- linkExpr <=< defineExpr $ expr
    lhs <- definePat pat rhs

    --cleanup subterms
    mapM popNS $ reverse terms
    mapM popDfdNS $ reverse headers

    --Push term, now that we've created it. This is necessary as other terms within the same list may refer to it.
    pushNodeNS lhs
    return $ Left lhs

defineDecl (HsFunBind [HsMatch _ name pats (HsUnGuardedRhs expr) decls]) = do
    args <- mapM defineArg pats

    --use the same ID as the header. This avoids issues related to shadowing.
    DfdHeader rootID _ <- resolveFunc $ HsVar $ UnQual name

    --headers are needed in case we have recursive functions
    let decls' = sortDecls decls
    headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
    terms <- mapM defineDecl $ decls'

    --link functions. <=< is the monadic composition operator (analogous to .)
    root <- linkExpr <=< defineExpr $ expr

    mapM popNS $ reverse terms
    mapM popDfdNS $ reverse headers
    mapM popNodeNS $ reverse args

    --id, name, returnType, isSync, root
    let name' = fromHsName name
    let args' = (flip map) args $ (\x -> (x, UndefinedType)) . nodeID . snd
    let res = DFD rootID name' args' UndefinedType False root
    pushDfdNS (name', res)
    return $ Right (name', res)

--generates/resolves nodes for expressions
defineExpr :: HsExp -> NodeGen DNode
defineExpr (HsVar (UnQual name)) = resolveNode $ fromHsName name
defineExpr (HsLit (HsInt val)) = do
    nodeID <- newId
    return $ DLiteral nodeID $ fromIntegral val

defineExpr (HsLet decls exp) = do
    let decls' = sortDecls decls
    headers <- (liftM catMaybes) . (mapM createDfdHeaders) $ decls'
    terms <- mapM defineDecl $ decls'                       --locals are pushed on creation

    root <- linkExpr <=< defineExpr $ exp
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
--TODO: add support for lambdas, etc.
resolveFunc :: HsExp -> NodeGen DFD
resolveFunc (HsVar name) = resolveDFD $ fromHsQName name

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

    declName :: HsDecl -> String
    declName (HsFunBind ((HsMatch _ n _ _ _):_)) = fromHsName n
    declName (HsPatBind _ (HsPVar n) _ _) = fromHsName n

    --these functions are for collecting a list of functions/variables which each declaration depends on
    declDeps :: HsDecl -> [String]
    declDeps (HsPatBind _ _ rhs _) = rhsDeps rhs
    declDeps (HsFunBind ms) = concatMap f ms where
        f (HsMatch _ _ _ rhs _) = rhsDeps rhs

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

--linking logic - replaces function headers with DFDs

linkFunc :: DFD -> NodeGen DFD
linkFunc (DfdHeader id _) = resolveIdDFD id
linkFunc x = return x

linkDFD :: DFD -> NodeGen DFD
linkDFD (DFD a b c d e root) = liftM (\x -> DFD a b c d e x) $ linkExpr root

linkExpr :: DNode -> NodeGen DNode
linkExpr (DVariable a b (Just e)) = liftM (\e' -> DVariable a b (Just e')) $ linkExpr e
linkExpr (DFunctionCall id f args) = liftM2 (\f' -> \a' -> DFunctionCall id f' a') (linkFunc f) (mapM linkExpr args)
linkExpr x = return x

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

