--This module contains code for converting ASTs to DFDs
module GenerateDFD (astToDfd) where

import Control.Monad.State
import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Text.Printf
import Language.Haskell.Syntax

import Common
import DfdDef


--TODO: need to store exported function list here
astToDfd :: HsModule -> DProgram
astToDfd (HsModule _ _ exportSpec _ decls) = evalState m initialNodeData where
    m = mapM (createDFD . cleanDecl) decls

--cleaning logic

--rewrites expressions to remove irrelevant syntactic differences. e.g. infix vs prefix application
--the following white-listed expressions are the only ones we will support
cleanExpr :: HsExp -> HsExp
cleanExpr exp@(HsVar _) = exp
cleanExpr exp@(HsLit _) = exp
cleanExpr (HsLet decls exp) = HsLet (map cleanDecl decls) $ cleanExpr exp
cleanExpr (HsApp e1 e2) = HsApp (cleanExpr e1) (cleanExpr e2)
cleanExpr (HsIf e1 e2 e3) = HsIf (cleanExpr e1) (cleanExpr e2) (cleanExpr e3)
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
cleanDecl (HsFunBind matches) = HsFunBind [resMatch] where
    --final value is for pattern exhaustion
    res = foldl cleanMatch m0 matches
    resMatch = res HsWildCard                                           --using HsWildCard to represent non-exhaustive pattern matching
    --initial value m0 is used for the outermost match
    (HsMatch src name pats _ _):_ = matches                             --using info from first match for the resultant match object
    m0 = \expr -> HsMatch src name pats (HsUnGuardedRhs expr) []
    args = take (length pats) genArgs

cleanDecl d = error $ "Unknown declaration: " ++ pshow d

--Cleans function matches. Used in folding.
--The result is a function of the expression returned if the pattern fails
--The left argument will have precedence over the right.
cleanMatch :: (HsExp -> HsMatch) -> HsMatch -> (HsExp -> HsMatch)
cleanMatch leftM (HsMatch _ _ pats rhs decls) = res where
    res = \elseExpr -> leftM $ HsIf patternsMatch trueExp elseExpr
    patternsMatch = foldl1 andConds $ (trueExpr:) $ map patternMatches $ zip genArgs pats
    expr = case rhs of
        HsUnGuardedRhs e -> cleanExpr e
        _ -> error "Guarded RHSs are not implemented yet"               --Guarded RHS can have multiple expressions?
    trueExp = HsLet (map cleanDecl decls) expr

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

--DFD generation logic

--convert a cleaned function (should have only a single match) to a DFD
--TODO: find a way to reuse the code from defineDecl. This can be done by generating simple argument names for the patterns (_arg_0, etc.) then inserting let statements to perform binding
createDFD :: HsDecl -> NodeGen DFD
createDFD (HsFunBind [HsMatch _ name pats (HsUnGuardedRhs expr) decls]) = do
    rootID <- newId

    args <- mapM defineArg pats
    pushNodeNS args

    --TODO: this code assumes that all declarations are variables, not functions
    terms <- liftM concat $ mapM defineDecl decls
    pushNodeNS terms

    root <- defineExpr expr
    popNodeNS terms
    popNodeNS args

    --id, name, returnType, isSync, root
    return $ DFD rootID (show name) (DUInt 8) False root

--defines a function argument. Similar to definePat, but without binding
defineArg :: HsPat -> NodeGen (String, DNode)
defineArg (HsPVar name) = do
    nodeID <- newId
    return $ (fromHsName name, DVariable nodeID (DUInt 8) Nothing)

--Generates a node for a variable bound to a pattern.
--This needs to be able to return multiple nodes, due to destructuring, etc.
--TODO: add support for tuples, etc.
definePat :: HsPat -> DNode -> NodeGen [(String, DNode)]
definePat (HsPVar name) value = do
    nodeID <- newId
    return $ [(fromHsName name, DVariable nodeID (DUInt 8) (Just value))]

--generates nodes for declarations. These can be either variables or functions in their own right. Returns namespace info.
--lhs = rhs where subterms
--TODO: add support for nested functions (requires additional logic to input shared variables as args)
--TODO: this code assumes that all declarations are variables, not functions
--TODO: this should be the top-level function called by astToDfd. This would centralize function gathering logic, and allow the use of global variables (via CAFs and patterns)
defineDecl :: HsDecl -> NodeGen [(String, DNode)]
defineDecl (HsPatBind _ pat (HsUnGuardedRhs expr) decls) = do
    --subterms
    terms <- liftM concat $ mapM defineDecl decls
    pushNodeNS terms

    --define the RHS, and bind it to the LHS
    rhs <- defineExpr expr
    lhs <- definePat pat rhs

    popNodeNS terms
    return lhs

--generates/resolves nodes for expressions
defineExpr :: HsExp -> NodeGen DNode
defineExpr (HsVar (UnQual name)) = resolveNode $ fromHsName name
defineExpr (HsLit (HsInt val)) = do
    nodeID <- newId
    return $ DLiteral nodeID $ fromIntegral val
defineExpr e = error $ "Failed to match expression: " ++ pshow e

--utility functions

--converts an export into an unqualified name
exportName :: HsExportSpec -> HsName
exportName (HsEVar (UnQual name)) = name
exportName e = error $ "Unknown exportSpec: " ++ pshow e

fromHsName :: HsName -> String
fromHsName (HsIdent x) = x
fromHsName (HsSymbol x) = x

--yields an infinite list of arg names
genArgs :: [HsName]
genArgs = map (\a -> HsIdent $ "_arg_" ++ (show a)) [0..]

--TODO: this should return an expression equal to True
trueExpr :: HsExp
trueExpr = unit_con
