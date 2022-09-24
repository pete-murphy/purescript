{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Ide.Usage
  ( findReexportingModules
  , directDependants
  , eligibleModules
  , applySearch
  , findUsages
  ) where

import           Protolude hiding (moduleName)

import           Control.Lens (preview, (^.))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import           Language.PureScript.Ide.State (getAllModules, getFileState)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

-- Garbage imports for debugging
import Data.Aeson (toJSON, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Lens.Fold ((^?))
import GHC.IO (unsafePerformIO)
import Control.Arrow ((&&&), (***))

-- |
-- How we find usages, given an IdeDeclaration and the module it was defined in:
--
-- 1. Find all modules that reexport the given declaration
-- 2. Find all modules that import from those modules, and while traversing the
-- imports build a specification for how the identifier can be found in the
-- module.
-- 3. Apply the collected search specifications and collect the results
findUsages
  :: (MonadIO m, Ide m)
  => IdeDeclaration
  -> P.ModuleName
  -> m (ModuleMap (NonEmpty P.SourceSpan))
findUsages declaration moduleName = do
  ideDeclarationAnnsByModuleName <- getAllModules Nothing
  modulesByModuleName <- Map.map fst . fsModules <$> getFileState
  let searchesByModuleName = eligibleModules (moduleName, declaration) ideDeclarationAnnsByModuleName modulesByModuleName
  let sourceSpansByModuleName = searchesByModuleName & Map.mapWithKey 
        \moduleName' searches -> do
          let maybeModule = Map.lookup moduleName' modulesByModuleName
          case maybeModule of
            Nothing -> []
            Just module' -> do
              let searchToSourceSpans = applySearch module'
                  sourceSpans = foldMap (uncurry (<>) . (searchToSourceSpans *** identity)) searches
                  -- sourceSpans = foldMap (uncurry (<>) . (searchToSourceSpans *** const [])) searches
              sourceSpans
  -- @TODO: Need to add to this
  --  - the definition site
  --  - all imports
  pure (Map.mapMaybe nonEmpty sourceSpansByModuleName)

-- | A declaration can either be imported qualified, or unqualified. All the
-- information we need to find usages through a Traversal is thus captured in
-- the `Search` type.
type Search = P.Qualified IdeDeclaration

findReexportingModules
  :: (P.ModuleName, IdeDeclaration)
  -- ^ The declaration and the module it is defined in for which we are
  -- searching usages
  -> ModuleMap [IdeDeclarationAnn]
  -- ^ Our declaration cache. Needs to have reexports resolved
  -> [P.ModuleName]
  -- ^ All the modules that reexport the declaration. This does NOT include
  -- the defining module
findReexportingModules (moduleName, declaration) decls =
  Map.keys (Map.filter (any hasReexport) decls)
  where
    hasReexport d =
      (d & _idaDeclaration & identifierFromIdeDeclaration) == identifierFromIdeDeclaration declaration
      && (d & _idaAnnotation & _annExportedFrom) == Just moduleName
      && (d & _idaDeclaration & namespaceForDeclaration) == namespaceForDeclaration declaration
-- @TODO: What does this do???
directDependants :: IdeDeclaration -> ModuleMap P.Module -> P.ModuleName -> ModuleMap (NonEmpty (Search, [P.SourceSpan]))
directDependants declaration modulesByModuleName moduleName = Map.mapMaybe (nonEmpty . go) modulesByModuleName
  where
    go :: P.Module -> [(Search, [P.SourceSpan])]
    go = foldMap isImporting . P.getModuleDeclarations

    isImporting :: P.Declaration -> [(P.Qualified IdeDeclaration, [P.SourceSpan])]
    isImporting d = case d of
      P.ImportDeclaration _ moduleName' importDeclarationType qual
        | moduleName == moduleName' -> first (P.Qualified (P.byMaybeModuleName qual)) <$> case importDeclarationType of
          P.Implicit -> do
            -- traceM "\n______________________"
            -- traceM "Implicit"
            -- traceShowM d
            pure (declaration, [])
          P.Explicit refs
            | any (isJust . spanOfRefMatching declaration) refs -> do
              -- traceM "\n______________________"
              -- traceM "P.Explicit refs with refs matching declaration"
              -- for_ refs (traceM . ("\n >>> " <>) . show)
              pure (declaration, mapMaybe (spanOfRefMatching declaration) refs)
          P.Explicit refs -> do
            -- traceM "\n______________________"
            -- traceM "P.Explicit refs with NO refs matching declaration"
            -- for_ refs (traceM . ("\n >>> " <>) . show)
            []
          P.Hiding refs
            | not (any (isJust . spanOfRefMatching declaration) refs) -> do
              -- traceM "\n______________________"
              -- traceM "P.Hiding refs"
              -- for_ refs (traceM . ("\n >>> " <>) . show)
              pure (declaration, mapMaybe (spanOfRefMatching declaration) refs)
          P.Hiding _ -> do
            traceM "Hiding"
            []
      _ -> []

-- | Determines whether an IdeDeclaration is referenced by a DeclarationRef.
--
-- TODO(Christoph): We should also extract the spans of matching refs here,
-- since they also count as a usage (at least for rename refactorings)
-- @TODO(Pete): we're now doing that (this function now returns 'Maybe P.SourceSpan'
-- instead of 'Bool') but what do we do with it?
spanOfRefMatching :: IdeDeclaration -> P.DeclarationRef -> Maybe P.SourceSpan
spanOfRefMatching declaration ref = case declaration of
  IdeDeclValue valueDecl -> case ref of
    P.ValueRef span i | i == _ideValueIdent valueDecl -> Just span
    _ -> Nothing
  IdeDeclType typeDecl -> case ref of
    P.TypeRef span tn what | tn == _ideTypeName typeDecl -> Just span
    _ -> Nothing
  IdeDeclTypeSynonym synonym -> case ref of
    P.TypeRef span tn _ | tn == _ideSynonymName synonym -> Just span
    _ -> Nothing
  IdeDeclDataConstructor dtor -> case ref of
    P.TypeRef span tn dtors
    -- We check if the given data constructor constructs the type imported
    -- here.
    -- This way we match `Just` with an import like `import Data.Maybe (Maybe(..))`
      | _ideDtorTypeName dtor == tn
        && maybe True (elem (_ideDtorName dtor)) dtors -> Just span
    _ -> Nothing
  IdeDeclTypeClass typeClass -> case ref of
    P.TypeClassRef span name | name == _ideTCName typeClass -> Just span
    _ -> Nothing
  IdeDeclValueOperator valueOperator -> case ref of
    P.ValueOpRef span opName | opName == _ideValueOpName valueOperator -> Just span
    _ -> Nothing
  IdeDeclTypeOperator typeOperator -> case ref of
    P.TypeOpRef span opName | opName == _ideTypeOpName typeOperator -> Just span
    _ -> Nothing
  IdeDeclModule m -> case ref of
    P.ModuleRef span mn | m == mn -> Just span
    _ -> Nothing

eligibleModules
  :: (P.ModuleName, IdeDeclaration)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap P.Module
  -> ModuleMap (NonEmpty (Search, [P.SourceSpan]))
eligibleModules query@(moduleName, declaration) decls modules =
  let
    searchDefiningModule = (P.Qualified P.ByNullSourcePos declaration, []) :| []
  in
    Map.insert moduleName searchDefiningModule $
      foldMap (directDependants declaration modules) (moduleName :| findReexportingModules query decls)

-- | Finds all usages for a given `Search` throughout a module
applySearch :: P.Module -> Search -> [P.SourceSpan]
applySearch module_ search =
  foldMap findUsageInDeclaration decls
  where
    decls = P.getModuleDeclarations module_
    findUsageInDeclaration =
      let
        (extr, _, _, _, _) = P.everythingWithScope mempty goExpr goBinder mempty mempty
      in
        extr mempty

    goExpr scope expr = case expr of
      P.Var sp i
        | Just ideValue <- preview _IdeDeclValue (P.disqualify search)
        , P.isQualified search
          || not (P.LocalIdent (_ideValueIdent ideValue) `Set.member` scope) ->
          [sp | map P.runIdent i == map identifierFromIdeDeclaration search]
      P.Constructor sp name
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | name == map _ideDtorName ideDtor]
      P.Op sp opName
        | Just ideOp <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName ideOp]
      _ -> []

    goBinder _ binder = case binder of
      P.ConstructorBinder sp ctorName _
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | ctorName == map _ideDtorName ideDtor]
      P.OpBinder sp opName
        | Just op <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName op]
      _ -> []
