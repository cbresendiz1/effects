{-# LANGUAGE TemplateHaskell, TypeInType, TypeFamilies, ScopedTypeVariables,
             GADTs, UndecidableInstances, InstanceSigs, TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-unticked-promoted-constructors #-}

module Data.Nat where

import qualified Prelude as P
import Prelude ( (.), ($) )
import Data.Singletons.TH
import Data.Singletons.Prelude
import qualified GHC.TypeLits as TL
import Control.Arrow ( first, second )
import qualified System.Random as R

$(singletons [d| data Nat = Z | S Nat deriving (P.Eq, P.Ord) |])

fromInteger :: P.Integer -> Nat
fromInteger 0 = Z
fromInteger n = S (fromInteger (n P.- 1))

(+) :: Nat -> Nat -> Nat
Z   + m = m
S n + m = S (n + m)

(-) :: Nat -> Nat -> Nat
n   - Z   = n
Z   - _   = P.error "negative nat"
S n - S m = n - m

(*) :: Nat -> Nat -> Nat
Z   * _ = Z
S n * m = m + (n * m)

mod :: Nat -> Nat -> Nat
n `mod` m
  | n P.< m     = n
  | P.otherwise = (n - m) `mod` m

toInteger :: Nat -> P.Integer
toInteger Z     = 0
toInteger (S n) = 1 P.+ toInteger n

instance P.Show Nat where
  show = P.show . toInteger

instance P.Read Nat where
  readsPrec p = P.map (first fromInteger) . P.readsPrec p

instance P.Enum Nat where
  toEnum = fromInteger . P.fromIntegral
  fromEnum = P.fromInteger . toInteger

type family U n where
  U 0 = Z
  U n = S (U (n TL.- 1))

instance P.Num Nat where
  -- These are not as silly as they look, due to scoping rules around
  -- method definitions for a qualified-imported class. Oh, Haskell.
  (+) = (+)
  (-) = (-)
  (*) = (*)
  abs n = n
  signum n = n
  fromInteger = fromInteger

instance P.Real Nat where
  toRational = P.toRational . toInteger

instance P.Integral Nat where
  a `quotRem` b = first fromInteger $ second fromInteger $
                  toInteger a `P.quotRem` toInteger b
  toInteger = toInteger

instance R.Random Nat where
  randomR (a, b) = first fromInteger . R.randomR (toInteger a, toInteger b)
  random = first fromInteger . R.random
