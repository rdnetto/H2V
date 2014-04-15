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

--convert a cleaned function (should have only a single match) to a DFD
createDFD :: HsDecl -> NodeGen DFD
createDFD (HsFunBind [HsMatch _ name pats (HsUnGuardedRhs expr) decls]) = do
    --id name, returnType, isSync, root
    rootID <- newId

    --TODO: need to write logic to generate DFD nodes
    tmpID <- newId
    let root = DLiteral tmpID 0

    return $ DFD rootID (show name) (DUInt 8) False root

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
    patternsMatch = foldl1 andConds $ filter (/= trueExpr) $ map patternMatches $ zip genArgs pats
    expr = case rhs of
        HsUnGuardedRhs e -> cleanExpr e
        _ -> error "Guarded RHSs are not implemented yet"               --Guarded RHS can have multiple expressions?
    trueExp = HsLet (map cleanDecl decls) expr

    --given multiple Boolean expressions, AND them together
    andConds :: HsExp -> HsExp -> HsExp
    andConds left right = HsInfixApp left (HsQVarOp $ UnQual $ HsIdent "&&") right

--yields a boolean expression which is true if the pattern is matched
--the argument is a 2-tuple specifying the new argument name and the pattern to be matched
patternMatches :: (HsName, HsPat) -> HsExp
patternMatches (_, HsPVar _) = trueExpr
patternMatches (name, HsPNeg pat) = HsApp (HsVar $ UnQual $ HsIdent "not") $ patternMatches (name, pat)
patternMatches (name, HsPLit lit) = HsInfixApp (HsVar $ UnQual name) (HsQVarOp $ UnQual $ HsSymbol "==") (HsLit lit)
patternMatches (name, pat) = error $ printf "Unknown pattern in %s:\n%s" (show name) (show pat)

--converts an export into an unqualified name
exportName :: HsExportSpec -> HsName
exportName (HsEVar (UnQual name)) = name
exportName e = error $ "Unknown exportSpec: " ++ pshow e

--yields an infinite list of arg names
genArgs :: [HsName]
genArgs = map (\a -> HsIdent $ "_arg_" ++ (show a)) [0..]

--TODO: this should return an expression equal to True
trueExpr :: HsExp
trueExpr = unit_con
