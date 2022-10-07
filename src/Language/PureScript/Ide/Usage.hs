{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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
import Control.Lens.Combinators (ifor_, itraverse_)
import Protolude.Partial (fromJust)
import Language.PureScript.Names (disqualify)
import Protolude.Error (error)

-- @TODO
matchingDeclarations :: IdeDeclaration -> [P.Declaration] -> [P.Declaration]
matchingDeclarations = \case
  IdeDeclValue valueDecl -> filter \case
    P.ValueDeclaration (P.ValueDeclarationData (span, _) ident _ _ _) -> ident == _ideValueIdent valueDecl
    P.TypeDeclaration (P.TypeDeclarationData (span, _) ident _) -> ident == _ideValueIdent valueDecl
    _ -> False
  _ -> \xs -> xs

declarationToSpan :: P.Declaration -> P.SourceSpan
declarationToSpan = \case
  P.ValueDeclaration (P.ValueDeclarationData (span, _) _ _ _ _) -> span
  P.TypeDeclaration (P.TypeDeclarationData (span, _) _ _) -> span
  _ -> error "TODO"
  

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
findUsages ideDeclaration moduleName = do
  ideDeclarationAnnsByModuleName <- getAllModules Nothing

  -- let moduleItWasDefinedIn = Map.lookup moduleName ideDeclarationAnnsByModuleName

  modulesByModuleName <- Map.map fst . fsModules <$> getFileState
  let searchesByModuleName = eligibleModules (moduleName, ideDeclaration) ideDeclarationAnnsByModuleName modulesByModuleName

  let moduleItWasDefinedIn = Map.lookup moduleName modulesByModuleName
  let additionalSpans = case moduleItWasDefinedIn of
    -- @NOTE: This is the module that has the definition
    -- declarations should have the value/type decl
    -- refs has export reference
        Just (P.Module _ _ _ declarations mbDeclarationRefs) -> declarationToSpan <$> matchingDeclarations ideDeclaration declarations 
        _ -> []
      -- ifor_ declarations \i declaration -> do
      --   -- traceM ("\n\n(declaration:" <> show i <> "):")
      --   -- traceShowM declaration
      --   case (declaration, ideDeclaration) of
      --     -- P.DataDeclaration (span, _) _ name z z'  -> pure ()
      --     -- @TODO: Recur here
      --     -- P.DataBindingGroupDeclaration _ -> pure ()
      --     -- P.TypeSynonymDeclaration (span, _) name _ _ -> pure ()
      --     -- P.KindDeclaration (span, _) _ name _ -> pure ()
      --     (P.TypeDeclaration (P.TypeDeclarationData (span, _) ident _), IdeDeclValue valueDecl)
      --       | ident == _ideValueIdent valueDecl -> do
      --         traceM "\n\n**** Type declaration"
      --         traceShowM ident
      --         traceM (P.displaySourceSpan "." span)
      --     (P.ValueDeclaration (P.ValueDeclarationData (span, _) ident _ _ _), IdeDeclValue valueDecl)
      --       | ident == _ideValueIdent valueDecl -> do
      --         traceM "\n\n**** Value declaration"
      --         traceShowM ident
      --         traceM (P.displaySourceSpan "." span)
      --     _ -> pure ()
      -- for_ mbDeclarationRefs \declarationRefs -> do
      --   ifor_ declarationRefs \i declarationRef -> do
      --     -- @NOTE: in here we have ref to the export
      --     for_ (spanOfRefMatching ideDeclaration declarationRef) \span -> do 
      --       traceM ("\n\n(declarationRef:" <> show i <> "):")
      --       traceM (P.displaySourceSpan "." span)
  
  let sourceSpansByModuleName = searchesByModuleName & Map.mapWithKey 
        \moduleName' searches -> do
          let maybeModule = Map.lookup moduleName' modulesByModuleName
          case maybeModule of
            Nothing -> []
            Just module' -> do
              let searchToSourceSpans = applySearch module'
                  sourceSpans = foldMap (uncurry (<>) . first (foldMap searchToSourceSpans)) searches
                  -- @NOTE: The above line _only_ gets us the import statement in addition
                  -- to what we would have got from:
                  --   foldMap (uncurry (<>) . (searchToSourceSpans *** const [])) searches
              sourceSpans
  -- @TODO: Need to add to this
  --  - the definition site
  --  - all imports
  let x = sourceSpansByModuleName & Map.insertWith (<>) moduleName additionalSpans
  pure (Map.mapMaybe nonEmpty x)

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
directDependants :: IdeDeclaration -> ModuleMap P.Module -> P.ModuleName -> ModuleMap (NonEmpty ([Search], [P.SourceSpan]))
directDependants declaration modulesByModuleName moduleName = Map.mapMaybe (nonEmpty . go) modulesByModuleName
  where
    go :: P.Module -> [([Search], [P.SourceSpan])]
    go = foldMap isImporting . P.getModuleDeclarations

    -- These spans are referring to just the spans in the import declaration
    isImporting :: P.Declaration -> [([P.Qualified IdeDeclaration], [P.SourceSpan])]
    isImporting d = pure case d of
      P.ImportDeclaration _ moduleName' importDeclarationType qual
        | moduleName == moduleName' -> map (P.Qualified (P.byMaybeModuleName qual)) `first` case importDeclarationType of
          P.Implicit -> do
            ([declaration], [])
          P.Explicit refs -> case mapMaybe (spanOfRefMatching declaration) refs of
            [] -> ([], [])
            spans -> ([declaration], spans)
          P.Hiding refs -> case mapMaybe (spanOfRefMatching declaration) refs of
            [] -> ([declaration], [])
            spans -> ([], spans)
      _ -> ([], [])

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
  -> ModuleMap (NonEmpty ([Search], [P.SourceSpan]))
eligibleModules query@(moduleName, declaration) decls modules = 
  let
    searchDefiningModule = ([P.Qualified P.ByNullSourcePos declaration], []) :| []
    mapx = foldMap (directDependants declaration modules) (moduleName :| findReexportingModules query decls)
  in  -- traceShow declaration do
          Map.insert moduleName searchDefiningModule mapx
            

-- | Finds all usages for a given `Search` throughout a module
applySearch :: P.Module -> Search -> [P.SourceSpan]
applySearch module_ search =
  foldMap findUsageInDeclaration decls
  where
    decls = P.getModuleDeclarations module_
    findUsageInDeclaration =
      let
        (extr, x1, x2, x3, x4) = P.everythingWithScope mempty goExpr goBinder mempty mempty
      in
        extr mempty

    goExpr scope expr = case expr of
      P.Var sp i
        | Just ideValue <- preview _IdeDeclValue (P.disqualify search)
        , P.isQualified search
          || not (P.LocalIdent (_ideValueIdent ideValue) `Set.member` scope) -> do
          -- traceShowM search
          -- traceM ("))))--  " <> show sp)
          [sp | map P.runIdent i == map identifierFromIdeDeclaration search]
      P.Constructor sp name
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search -> do
          [sp | name == map _ideDtorName ideDtor]
      P.Op sp opName
        | Just ideOp <- traverse (preview _IdeDeclValueOperator) search -> do
          [sp | opName == map _ideValueOpName ideOp]
      _ -> []

    goBinder _ binder = case binder of
      P.ConstructorBinder sp ctorName _
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | ctorName == map _ideDtorName ideDtor]
      P.OpBinder sp opName
        | Just op <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName op]
      P.VarBinder x y -> do
        -- traceShowM x
        []
      _ -> []
