import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Exception.Base

import qualified Data.Map as Map

import TauPP
import TauParser
import TauExec
import TauSerializer

patternMatch (PatternMatchFail _) = True

runMain ast = (case Map.lookup "main" ast of Just x -> serialize $ exec x
                                             Nothing -> "")


main :: IO ()
main = hspec $ do
  describe "Parse&eval" $ do

    it "parse and eval identity" $ do
      (serialize $ exec $ parse "(a => a) U") `shouldBe` "U"

    it "parse and eval apply with FCOMP" $ do
      (serialize $ exec $ parse "( (a b => b a) R U)") `shouldBe` "(U R)"

    it "parse and eval partial apply with FCOMP 2/2 params" $ do
      (exec $ parse "( (a b => b a) _ Rx)") `shouldBe` FCOMP (ARGS ["a"]) (APPLY (FCOMP (ARGS ["a","b"]) (APPLY (ID "b") [ID "a"])) [ID "a",ID "Rx"])

    it "parse and eval partial apply with FCOMP 2/3 params" $ do
      (exec $ parse "( (a b c => b c a) Rx _)") `shouldBe` (FCOMP (ARGS ["b","c"]) (APPLY (FCOMP (ARGS ["a","b","c"]) (APPLY (ID "b") [ID "c",ID "a"])) [ID "Rx",ID "b",ID "c"]))

    it "parse and eval apply with more args then expected" $ do
      evaluate (exec $ parse "( (a b => b a) u Rx e)") `shouldThrow` anyException

    it "parse and eval apply (FCOMP) with duplicates in args" $ do
      evaluate (exec $ parse "(a a => a a) a a") `shouldThrow` patternMatch

    it "parse and eval apply (FDEF) with duplicates in args" $ do
      evaluate (exec $ parse "(a a => match a | O => O) O O") `shouldThrow` patternMatch

    it "parse and eval apply (FDEF) with defined match parameter" $ do
      (serialize $ exec $ parse "(a => match a | O => O) O") `shouldBe` "O"

    it "parse and eval apply (FDEF) with undefined match parameter" $ do
      evaluate (exec $ parse "(a => match b | O => O) O") `shouldThrow` patternMatch

    describe "match" $ do

        it "parse and eval apply (FDEF) with proper clause" $ do
          (serialize $ exec $ parse "\
\         (\
\              (a => match a\
\                  | S a' => O\
\              ) (S O)\
\         )")  `shouldBe` "O"

        it "parse and eval apply (FDEF) without proper clause" $ do
            evaluate (exec $ parse "\
\           (\
\               (a => match a\
\                   | S a' => O\
\                   | S b' => E\
\               ) (R O)\
\           )")  `shouldThrow` patternMatch

        it "parse and eval apply (FDEF) with 2 same clauses" $ do
            evaluate (exec $ parse "\
\           (\
\               (a => match a\
\                   | S a' => O\
\                   | S b' => E\
\               ) (S O)\
\           )")  `shouldThrow` patternMatch

        it "parse and eval apply (FDEF) with same params names in different clauses" $ do
            (serialize $ exec $ parse "\
\           (\
\               (a => match a\
\                   | S a' => O\
\                   | R a' => O\
\               ) (S O)\
\           )")  `shouldBe` "O"

        it "parse and eval apply (FDEF) with duplicate param in destructing" $ do
            evaluate (exec $ parse "\
\           (\
\               (a => match a\
\                   | S a => O\
\               ) (S O)\
\           )")  `shouldThrow` patternMatch

-- end of match

    describe "polymorphic match" $ do
        it "parse and eval apply (FDEF) with polymorphic match" $ do
            (serialize $ exec $ parse "\
\           (\
\               (a => match a\
\                   | S a' => O\
\                   | _ a' => _\
\               ) (R O)\
\           )")  `shouldBe` "R"

        it "parse and eval apply (FDEF) with polymorphic match and scip parameter in destructing" $ do
            (serialize $ exec $ parse "\
\           (\
\               (a => match a\
\                   | S a' => O\
\                   | _ _ => _\
\               ) (R O)\
\           )")  `shouldBe` "R"

        it "parse and eval apply (FDEF) with polymorphic match and scip parameter in destructing" $ do
            (serialize $ exec $ parse "\
\           (\
\               (a => match a\
\                   | S a' => _\
\               ) (S O)\
\           )")  `shouldBe` "_"

        it "parse and eval polimorphic match on primitive CGT" $ do
            (serialize $ exec $ parse "( a => (\
\                       (double a => double double a)\
\                       (double a => match a\
\                           | _ => _ _\
\                       )\
\                       a\
\                       )\
\    ) R") `shouldBe` "(R R)"


        it "parse and eval polimorphic match on complex CGT" $ do
            (serialize $ exec $ parse "( a => (\
\                       (double a => double double a)\
\                       (double a => match a\
\                           | O => O\
\                           | _ a' => ( _ ( _ (double double a')))\
\                       )\
\                       a\
\                       )\
\    ) (R (R O))") `shouldBe` "(R (R (R (R O))))"

{-
    it "parse and eval apply (FDEF) with polymorphic match and post resolve" $ do
      (serialize $ exec $ parse "\
\      (\
\           (scipper a => match a\
\               | S a' => O\
\               | _ a' => scipper a'\
\           ) (a => _ a) (R O)\
\       )")  `shouldBe` "R"
-}
-- end of polymorphic match


    it "parse and eval partial apply FCOMP/FDEF" $ do
      (serialize $ exec $ parse "\
\      (\
\           (sum a b => sum sum a b)\
\           Rx\
\           _\
\           (S O)\
\       )") `shouldBe` "(a => ((sum a b => (sum sum a b)) Rx a (S O)))"





    it "parse and eval partial apply FCOMP/FDEF with match arg" $ do
      (serialize $ exec $ parse "\
\      (\
\           (sum a b => sum a b)\
\           (a b => match a\
\               | O => b\
\               | S a' => Rx a b\
\           )\
\           _\
\           O\
\       )") `shouldBe` "(a => ((sum a b => (sum a b)) (a b => match a | O  => b | S a' => (Rx a b)) a O))"

    it "parse and eval (sum 2 1)" $ do
      (serialize $ exec $ parse "\
\      (\
\           (sum a b => sum sum a b)\
\           (sum a b =>\
\               match a\
\               | O => b\
\               | S a' => (S (sum sum a' b))\
\           )\
\           (S (S O))\
\           (S O)\
\       )") `shouldBe` "(S (S (S O)))"

    it "parse and eval (sum 2 1) with partial apply" $ do
      (serialize $ exec $ parse "\
\      ((\
\           (sum a b => sum sum a b)\
\           (sum a b =>\
\               match a\
\               | O => b\
\               | S a' => (S (sum sum a' b))\
\           )\
\           (S (S O))\
\       )\
\       (S O)\
\       )") `shouldBe` "(S (S (S O)))"

  describe "Preprocessor" $ do
    it "load module and run main (ID O)" $ do
      ast <- getModule "Tests.01"
      (runMain ast) `shouldBe` "O"

  describe "etc" $ do

    it "load module and run: quicksort" $ do
      ast <- getModule "Tests.02"
      (runMain ast) `shouldBe` "(L (S O) (L (S (S (S O))) (L (S (S (S (S O)))) (L (S (S (S (S (S O))))) []))))"
