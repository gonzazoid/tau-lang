import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TauParser
import TauExec
import TauSerializer

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
{-    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
-}



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

    it "parse and eval partial apply FCOMP/FDEF" $ do
      (serialize $ exec $ parse "\
\      (\
\           (sum a b => sum sum a b)\
\           Rx\
\           _\
\           (S O)\
\       )") `shouldBe` "(a => ((sum a b => (sum sum a b)) Rx a (S O)))"

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

    it "parse and eval quick sort" $ do
      (serialize $ exec $ parse "\
\    (\
\    ( list =>\
\        (sort join filter lt ge list => sort sort join filter lt ge list)\
\        (sort join filter lt ge list =>\
\            match list\
\            | EMPTY => EMPTY\
\            | L head tail => join (sort sort join filter lt ge (filter tail (lt _ head))) (L head (sort sort join filter lt ge (filter tail ( ge _ head))))\
\        )\
\        (list1 list2 =>\
\            (join list1 list2 => join join list1 list2)\
\            ( join list1 list2 =>\
\                match list1\
\                | EMPTY => list2\
\                | L head tail => L head (join join tail list2)\
\            )\
\           list1\
\           list2\
\       )\
\       (list predicate =>\
\           ( filter filter' predicate list =>\
\               match list\
\               | EMPTY => EMPTY\
\               | L head tail => filter' ( filter filter filter' predicate tail ) head (predicate head)\
\           )\
\           ( filter filter' predicate list =>\
\               match list\
\               | EMPTY => EMPTY\
\               | L head tail => filter' ( filter filter filter' predicate tail ) head (predicate head)\
\           )\
\           ( list elem flag =>\
\               match flag\
\               | true => L elem list\
\               | false => list\
\           )\
\           predicate\
\           list\
\       )\
\       ( a b =>\
\           (lt lt' a b => lt lt lt' a b)\
\           ( lt lt' a b =>\
\               match b\
\               | O => false\
\               | S b' => lt' lt' lt a b'\
\           )\
\           (lt' lt a b' =>\
\               match a\
\               | O => true\
\               | S a' => lt lt lt' a' b'\
\           )\
\           a\
\           b\
\       )\
\       ( a b =>\
\           (ge ge' isZero a b => ge ge ge' isZero a b)\
\           ( ge ge' isZero a b =>\
\               match a\
\               | O => isZero b\
\               | S a' => ge' ge' ge isZero a' b\
\           )\
\           (ge' ge isZero a' b =>\
\               match b\
\               | O => true\
\               | S b' => ge ge ge' isZero a' b'\
\           )\
\           (x =>\
\               match x\
\               | O => true\
\               | S x' => false\
\           )\
\           a\
\           b\
\       )\
\       list\
\   )\
\   (L (S(S(S(O)))) (L (S(S(S(S(S(O)))))) (L (S(S(S(S(O))))) (L (S(S O)) EMPTY))))\
\   )" ) `shouldBe` "(L (S (S O)) (L (S (S (S O))) (L (S (S (S (S O)))) (L (S (S (S (S (S O))))) EMPTY))))"
