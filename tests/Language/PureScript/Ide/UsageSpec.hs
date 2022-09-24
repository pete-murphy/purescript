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

-- displaySourceSpan :: P.SourceSpan -> Text
-- displaySourceSpan P.SourceSpan {spanName, spanStart, spanEnd}
--    = (Text.pack . last) (splitOn "/" spanName) <> ":" <> P.displaySourcePosShort spanStart <> "-" <> P.displaySourcePosShort spanEnd

spec :: Spec
spec = describe "Finding Usages" $ do
    it "finds a simple usage" $ do
      (xs, _) <- Test.inProject $
        Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
                    , usage (Test.mn "FindUsage.Definition") "usageId" IdeNSValue
                    ]
      dir <- getCurrentDirectory
      forOf_ (folded . _Right . _UsagesResult . folded) xs (traceM . P.displaySourceSpan dir)
      
      pure ()
      -- ([_, Right (UsagesResult [usage3, usage4])], _) <- Test.inProject $
      --   Test.runIde [ load ["FindUsage", "FindUsage.Definition", "FindUsage.Reexport"]
      --               , usage (Test.mn "FindUsage") "usageId" IdeNSValue
      --               ]
      -- usage1 `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "9:1-9:8")
      -- usage2 `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "10:1-10:8")
      -- usage3 `shouldBeUsage` ("src" </> "FindUsage.purs", "12:11-12:18")
      -- usage4 `shouldBeUsage` ("src" </> "FindUsage" </> "Definition.purs", "13:18-13:25")
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
