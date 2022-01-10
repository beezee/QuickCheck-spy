import Control.Exception
import Data.Maybe (mapMaybe)
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property
import Test.QuickCheck.Spy

prop_noFailSpy :: Spy Int String -> [Int] -> Property
prop_noFailSpy s ints = monadicIO $ do
  (ref, fn) <- run . unSpy $ s
  res <- run . traverse fn $ ints
  spied <- run . getSpied $ ref
  _ <- return $ 
      if res == (snd <$> spied) then succeeded 
      else failed { reason = show res <> " /= " <> (show $ snd <$> spied) }
  return $ 
      if ints == (fst <$> spied) then succeeded
      else failed { reason = show ints <> " /= " <> (show $ fst <$> spied) }

newtype Af = Af AssertionFailed

instance MockThrow Af where
  type UnMockThrow Af = AssertionFailed
  unMockThrow (Af e) = e

instance Arbitrary Af where
  arbitrary = Af . AssertionFailed <$> arbitrary

prop_failSpy :: IO ()
prop_failSpy = quickCheck $ \s ints -> monadicIO $ do
  (ref, fn) <- run . throwSpy @10 @Af $ s
  res <- run . traverse @_ @_ @Int @(Either AssertionFailed String) (try . fn) $ ints
  spied <- run . getSpied $ ref
  let success = mapMaybe rightToMaybe $ res
  _ <- return $ 
      if success == (snd <$> spied) then succeeded 
      else failed { reason = show success <> " /= " <> (show $ snd <$> spied) }
  let skippedInputs = checkInputs (fst <$> spied) ints
  return $ 
      if skippedInputs == [] then succeeded
      else failed { reason = "Recorded outside of given inputs " <> (show skippedInputs) }
  where
    rightToMaybe (Right a) = Just a
    rightToMaybe (Left _) = Nothing

    checkInputs [] _ = []
    checkInputs x [] = x
    checkInputs (h1:t1) (h2:t2) = 
      let (rec1, rec2) = if h1 == h2 then (t1, t2) else (h1:t1, t2)
      in checkInputs rec1 rec2

main :: IO ()
main = 
  quickCheck prop_noFailSpy >> prop_failSpy
