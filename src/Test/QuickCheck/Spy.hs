{-# LANGUAGE FlexibleContexts #-}
module Test.QuickCheck.Spy where

import Control.Exception (Exception, throw)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import Test.QuickCheck (CoArbitrary, Arbitrary (..), frequency)

-- TODO - consider whether this shifts to allow recording Either e b on results when throwing
type UnSpy a b = IO (IORef ([(a, b)], Maybe a), a -> IO b)
newtype Spy a b = Spy (UnSpy a b)

instance Show (Spy a b) where
  show _ = "<Spy>"

unSpy :: Spy a b -> UnSpy a b
unSpy (Spy io) = io

getSpied :: IORef ([(a, b)], Maybe a) -> IO [(a, b)]
getSpied = (reverse . fst <$>) . readIORef

makeSpy :: (a -> IO b) -> Spy a b
makeSpy fn = Spy $ do
  ref <- newIORef ([], Nothing)
  return (ref, spyOn ref fn)
  where
    spyOn r f input = do
      modifyIORef r (\(c, _) -> (c, Just input))
      result <- f input
      modifyIORef r (\(c, i) -> ((input, result):c, i))
      return result

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Spy a b) where
  arbitrary = makeSpy . (return .) <$> arbitrary

newtype DistEither (f :: Nat) e a = DistEither (Either e a)

unDistEither :: DistEither f e a -> Either e a
unDistEither (DistEither e) = e

instance (KnownNat f, Arbitrary e, Arbitrary a) => Arbitrary (DistEither f e a) where
  arbitrary =
    DistEither <$> frequency [
      (freq, Left <$> arbitrary), 
      (max 0 $ 100 - freq, Right <$> arbitrary)]
    where
      freq = fromIntegral . natVal $ Proxy @f

newtype ThrowSpy (f :: Nat) n a b e = ThrowSpy (Spy a b)

instance Show (ThrowSpy f n a b e) where
  show _ = "<ThrowSpy>"

throwSpy :: ThrowSpy f n a b e -> UnSpy a b
throwSpy (ThrowSpy s) = unSpy s

class MockThrow n where
  type UnMockThrow n
  unMockThrow :: n -> UnMockThrow n

instance (
  e ~ UnMockThrow n, KnownNat f, CoArbitrary a, Arbitrary n, 
  Arbitrary b, MockThrow n, Exception e) => 
    Arbitrary (ThrowSpy f n a b e) where
      arbitrary = 
        ThrowSpy . makeSpy 
          . ((throwLeft . unDistEither) .) <$> arbitrary @(a -> DistEither f n b)
        where
          throwLeft (Left n) = throw . unMockThrow $ n
          throwLeft (Right a) = return a
