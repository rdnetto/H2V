--This module contains code for converting ASTs to DFDs
module GenerateDFD (astToDfd) where

import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Text.Printf
import Language.Haskell.Syntax

import Common
import DfdDef


astToDfd :: HsModule -> DProgram
astToDfd (HsModule _ _ exportSpec _ decls) = do
    --TODO: do this monadically, so that each DFD and node has a unique ID
    return mapM (createDFD . cleanDecl) decls

--convert a cleaned function (should have only a single match) to a DFD
--TODO: redefine this to be monadic
createDFD :: HsDecl -> NodeGen DFD
createDFD (HsFunBind [HsMatch _ name pats (HsUnGuardedRhs expr) decls]) = do
    --TODO: traverse AST and generate DFD structure
    --pats will define argument names, expr is our starting point
    --should probably generate IDs at the same time, since this is the easiest way of doing so


    return $ DFD name (DUInt 8) False root







--converts an AST to a DFD
astToDfd_old :: HsModule -> DFD
astToDfd_old (HsModule _ hMod exportSpec _ decls) = DFD coreFuncs allFuncs where
    coreFuncs = case exportSpec of
                Just exports -> map exportName $ exports
                Nothing -> [HsIdent "main"]                         --if there is no export spec, assume the function is called main
    allFuncs = map processFunc decls

--generates the DFD object for a function
--OBSOLETE - remove once code is working
processFunc :: HsDecl -> Function
processFunc (HsFunBind matches) = Function name args expr where
    name = case matches of                                          --use the name from the first match clause
           (HsMatch _ name _ _ _):_ -> name

    --a list of argument names wrapped in Maybe to handle undefined names
    args' = map firstPVar $ transpose $ map getPatterns matches where
        getPatterns (HsMatch _ _ p _ _) = p                         --get the arg names from a match clause
        firstPVar :: [HsPat] -> Maybe HsName                        --returns the first defined name for an argument
        firstPVar (HsPVar arg:_) = Just arg
        firstPVar (_:xs) = firstPVar xs
        firstPVar [] = Nothing

    --replace the null names with generated ones
    args = map f $ zip [0..] args' where
        f (i, Nothing) = HsIdent ("arg_" ++ show i)
        f (_, Just x) = x

    --add match selection logic to expression tree
    expr = cleanExpr $ selectMatch $ cleanMatch matches

    --selects the match expression to use, and wrap it in a HsLet to preserve where-declarations
    selectMatch :: [HsMatch] -> HsExp
    selectMatch [HsMatch _ name pats (HsUnGuardedRhs exp) decls] = HsLet newDecls exp where      --base case: only one match
        argIDs = map (\x -> HsIdent $ "arg_" ++ show x) [0..]
        args = map fromJust $ filter isJust $ map patternDecls $ zip argIDs pats
        newDecls = (args ++ decls)

    selectMatch [m] = error $ "Failed to match:" ++ pshow m

    --TODO: need to handle pattern matching & destructuring
    selectMatch (m0:ms) = HsIf cond (selectMatch [m0]) (selectMatch ms) where     --perform pattern matching via IFs
        --this technically makes variables accessible where they shouldn't be, but if they are used they'll get shadowed anyway
        HsMatch _ _ pats _ _ = m0       --there is one HsPat for each argument
        --AND together conditions which are not always true
        argIDs = map (\x -> "arg_" ++ show x) [0..]
        cond = combineConds $ map fromJust $ filter isJust $ map patternMatches $ zip argIDs pats

    --returns an expression which is True iff the pattern matches
    --takes an (argument name, pattern) tuple
    --returns Nothing if the pattern will always match
    patternMatches :: (String, HsPat) -> Maybe HsExp
    patternMatches (_, HsPVar _) = Nothing
    patternMatches (_, HsPWildCard) = Nothing
    patternMatches (argId, HsPLit lit@(HsInt val)) = Just $ cleanExpr $ HsInfixApp lhs op rhs where
        lhs = HsVar $ UnQual $ HsIdent argId
        op = HsQVarOp $ UnQual $ HsSymbol "=="
        rhs = HsLit lit
    patternMatches (arg, pat) = error $ printf "%s: unknown pattern%s" arg (pshow pat)

    combineConds :: [HsExp] -> HsExp
    combineConds [x] = x

    --extracts bindings from pattern matching
    patternDecls :: (HsName, HsPat) -> Maybe HsDecl
    patternDecls (argID, var@(HsPVar _)) = Just $ HsPatBind (SrcLoc "" 0 0) var (HsUnGuardedRhs $ HsVar $ UnQual argID) []
    patternDecls (_, HsPLit _) = Nothing
    patternDecls (_, HsPWildCard) = Nothing
    patternDecls (arg, pat) = error $ printf "%s: unknown pattern%s" (show arg) (pshow pat)

processFunc d = error $ "Unknown declaration: " ++ pshow d

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
--TODO: implement pattern matching and destructuring logic here
cleanDecl :: HsDecl -> HsDecl
cleanDecl (HsPatBind src pat rhs decls) = HsPatBind src pat (cleanRHS rhs) (map cleanDecl decls)
--cleanDecl (HsFunBind matches) = HsFunBind (map cleanMatch matches)
cleanDecl (HsFunBind matches) = HsFunBind [resMatch] where
    (HsMatch src name pats rhs decls):_ = matches
    resMatch = HsMatch src name genPats (HsUnGuardedRhs expr) []
    expr = cleanExpr $ selectMatch matches

cleanDecl d = error $ "Unknown declaration: " ++ pshow d

--cleans RHSs
--TODO: this should only handle unguarded RHSs. Guarded RHSs should be refactored at the pattern level
cleanRHS :: HsRhs -> HsRhs
cleanRHS (HsUnGuardedRhs exp) = HsUnGuardedRhs (cleanExpr exp)
cleanRHS (HsGuardedRhss guards) = HsGuardedRhss (map f guards) where
    f (HsGuardedRhs src e1 e2) = HsGuardedRhs src (cleanExpr e1) (cleanExpr e2)

--cleans function matches
cleanMatch :: HsMatch -> HsMatch
cleanMatch (HsMatch src name pats rhs decls) = HsMatch src name pats (cleanRHS rhs) (map cleanDecl decls)

--converts an export into an unqualified name
exportName :: HsExportSpec -> HsName
exportName (HsEVar name) = unqual name
exportName e = error $ "Unknown exportSpec: " ++ pshow e

--convert a qualified name to an unqualified name
unqual :: HsQName -> HsName
unqual (UnQual n) = n
unqual q = error $ "Unknown HsQName: " ++ pshow q
