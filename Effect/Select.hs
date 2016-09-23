-- adapted from Idris's algebraic effects library

{-# LANGUAGE TypeInType, GADTs, TypeFamilies, FlexibleInstances,
             MultiParamTypeClasses, ScopedTypeVariables, TypeApplications,
             AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors
                -Wno-name-shadowing #-}

module Effect.Select where

import Effects
import Data.Singletons

data Selection :: Effect where
  Select :: [a] -> Selection () () a

data instance Sing (x :: Selection a b c) where
  SSelect :: Sing xs -> Sing (Select xs)

instance (Good a, Good b, Good c) => SingKind (Selection a b c) where
  type DemoteRep (Selection a b c) = Selection a b c

  fromSing (SSelect xs) = Select (fromSing xs)

  toSing (Select xs) = case toSing xs of SomeSing xs' -> SomeSing (SSelect xs')

instance Handler Selection Maybe where
  handle (Select xs) _ k = tryAll xs where
    tryAll [] = Nothing
    tryAll (x : xs) = case k () x of
                        Nothing -> tryAll xs
                        Just v  -> Just v

instance Handler Selection [] where
  handle (Select xs) r k = concatMap (k r) xs

type SELECT = MkEff () Selection

select_ :: Good a => [a] -> Eff m '[SELECT] a
select_ xs = case toSing xs of SomeSing xs' -> Effect SHere (SSelect xs')

select :: forall xs prf a m.
          (SingI (prf :: SubList '[SELECT] xs), Good a)
       => [a] -> EffM m xs (UpdateWith '[SELECT] xs prf) a
select xs = lift @_ @_ @prf (select_ xs)
