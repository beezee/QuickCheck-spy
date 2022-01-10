import Control.Exception
import Data.Maybe (mapMaybe)
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property
import Test.QuickCheck.Spy

prop_spyDeterministic :: IO ()
prop_spyDeterministic = quickCheck $ \s ints -> monadicIO $ do
  (_, fn) <- run . unSpy $ s
  res <- run . traverse @[] @_ @Int @String fn $ ints
  res2 <- run . traverse fn $ ints 
  return $ 
    if res == res2 then succeeded
    else failed { reason = show res <> " /= " <> show res2 }

prop_noFailSpy :: IO ()
prop_noFailSpy = quickCheck $ \s ints -> monadicIO $ do
  (ref, fn) <- run . unSpy $ s
  res <- run . traverse @_ @_ @Int @String fn $ ints
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

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right a) = Just a
rightToMaybe (Left _) = Nothing

prop_failZeroSpy :: IO ()
prop_failZeroSpy = quickCheck $ \s ints -> monadicIO $ do
  (ref, fn) <- run . throwSpy @0 @Af $ s
  eres <- run . traverse @_ @_ @Int @(Either AssertionFailed String) (try . fn) $ ints
  spied <- run . getSpied $ ref
  let res = mapMaybe rightToMaybe $ eres
  _ <- return $ 
    if res == (snd <$> spied) then succeeded 
    else failed { reason = show res <> " /= " <> (show $ snd <$> spied) }
  return $ 
    if ints == (fst <$> spied) then succeeded
    else failed { reason = show ints <> " /= " <> (show $ fst <$> spied) }

prop_failSpyDeterministic :: IO ()
prop_failSpyDeterministic = quickCheck $ \s ints -> monadicIO $ do
  (_, fn) <- run . throwSpy @50 @Af $ s
  res <- run . traverse @[] @_ @Int @(Either AssertionFailed String) (try . fn) $ ints
  res2 <- run . traverse (try @AssertionFailed . fn) $ ints 
  return $ 
    if (show res) == (show res2) then succeeded
    else failed { reason = show res <> " /= " <> show res2 }

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
    checkInputs [] _ = []
    checkInputs x [] = x
    checkInputs (h1:t1) (h2:t2) = 
      let (rec1, rec2) = if h1 == h2 then (t1, t2) else (h1:t1, t2)
      in checkInputs rec1 rec2

main :: IO ()
main = do
  prop_spyDeterministic
  prop_noFailSpy
  prop_failZeroSpy
  prop_failSpyDeterministic
  prop_failSpy
