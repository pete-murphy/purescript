{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Ide.UsageSpec where

import           Protolude

import qualified Data.Text as Text
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Ide.Test as Test
import qualified Language.PureScript as P
import           Test.Hspec
import           Data.Text.Read (decimal)
import           System.FilePath

-- Garbage imports for debugging
import Control.Lens.Operators ((^?), (^.), (^..))
import Control.Lens.Combinators
import Data.String (String)
import System.Directory (getCurrentDirectory)

load :: [Text] -> Command
load = LoadSync . map Test.mn

usage :: P.ModuleName -> Text -> IdeNamespace -> Command
usage = FindUsages

shouldBeUsage :: P.SourceSpan -> (FilePath, Text) -> Expectation
shouldBeUsage usage' (fp, range) =
  let
    [ start, end] = Text.splitOn "-" range
    unsafeReadInt = fst . either (panic "") identity . decimal
    [ startLine, startColumn ] = map unsafeReadInt (Text.splitOn ":" start)
    [ endLine, endColumn ] = map unsafeReadInt (Text.splitOn ":" end)
  in
    do
      projectDir <- Test.getProjectDirectory
      projectDir </> fp `shouldBe` P.spanName usage'

      (P.sourcePosLine (P.spanStart usage'), P.sourcePosColumn (P.spanStart usage'))
        `shouldBe`
        (startLine, startColumn)

      (P.sourcePosLine (P.spanEnd usage'), P.sourcePosColumn (P.spanEnd usage'))
        `shouldBe`
        (endLine, endColumn)

_UsagesResult :: Prism' Success [P.SourceSpan]
_UsagesResult = prism' UsagesResult \case
  UsagesResult xs -> Just xs
  _ -> Nothing

spec :: Spec
spec = describe "Finding Usages" $ do
    xit "finds a simple usage (NEW)" $ do

      ([_, Right (UsagesResult
        [ usage1
        , import1
        , usage2
        -- -- These cases aren't yet passing:
        -- , export1
        -- , defType
        -- , def
        ])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "usageId" IdeNSValue
                    ]
     
      -- Import/export cases
      import1 `shouldBeUsage` ("src" </> "FindUsage.purs", "3:30-3:37")
      -- export1 `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "1:47-1:54")

      -- Where it's defined
      -- deftype `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "9:1-9:8")
      -- def `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "10:1-10:8")
     
      -- Existing usage cases
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:11-12:18")
      usage2 `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "13:18-13:25")

    it "finds a simple usage" $ do

      ([_, Right (UsagesResult xs)], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "usageId" IdeNSValue
                    ]
      for_ xs (traceM . ("\n*** " <>) . P.displaySourceSpan ".")
     
    xit "finds a simple recursive usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage.Recursive"]
                    , usage (Test.mn "FindUsage.Recursive") "recursiveUsage" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage" </> "Recursive.purs", "7:12-7:26")
    xit "ignores a locally shadowed recursive usage" $ do
      ([_, Right (UsagesResult usageResult)], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage.RecursiveShadowed"]
                    , usage (Test.mn "FindUsage.RecursiveShadowed") "recursiveUsage" IdeNSValue
                    ]
      usageResult `shouldBe` []
    xit "finds a constructor usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "Used" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "8:3-8:9")
    xit "finds a constructor alias usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "$%" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "9:5-9:7")
    xit "finds a reexported usage" $ do
      ([_, Right (UsagesResult [usage1])], _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Reexport") "toBeReexported" IdeNSValue
                    ]
      usage1 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:19-12:33")
