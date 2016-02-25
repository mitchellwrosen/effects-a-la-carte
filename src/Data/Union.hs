{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Data.Union where

import GHC.Exts (Constraint)


-- | An open union data type that supports injection and projection, where the
-- summands happen to have kind @* -> *@.
--
-- Think of 'Union' as a growable and shrinkable 'Either' type, as in
--
-- > Either a b c ...
--
-- which supports injection,
--
-- @
-- inj :: a -> Either a b c ...
-- inj :: b -> Either a b c ...
-- inj :: c -> Either a b c ...
-- @
--
-- projection,
--
-- @
-- prj :: Either a b c ... -> Maybe a
-- prj :: Either a b c ... -> Maybe b
-- prj :: Either a b c ... -> Maybe c
-- @
--
-- and decomposition
--
-- @
-- decomp :: Either a b c ... -> Either (Either b c ...) a
-- decomp :: Either a b c ... -> Either (Either a c ...) b
-- decomp :: Either a b c ... -> Either (Either a b ...) c
-- @
--
data Union (fs :: [* -> *]) (x :: *) where
  Union :: f x -> Index f fs -> Union fs x


-- | A proof of the location of a type in a list of types.
data Index (x :: k) (xs :: [k]) where
  IZ :: Index x (x ': xs)
  IS :: Index x xs -> Index x (y ': xs)

-- | A proof that either two types are equal or not.
data a :==?: b where
  (:==:) :: a :==?: a
  (:/=:) :: a :==?: b

-- | Get a proof of whether or not two indices into the same list are equal.
indexEq :: Index x zs -> Index y zs -> x :==?: y
indexEq IZ IZ = (:==:)
indexEq (IS i) (IS j) = indexEq i j
indexEq _ _ = (:/=:)


-- | List membership multiparameter typeclass. There are only two (overlapping)
-- instances; no others should ever be written.
class (x :: k) :< (xs :: [k]) where
  index :: Index x xs

instance {-# OVERLAPS #-} x :< (x ': xs) where
  index = IZ

instance {-# OVERLAPS #-} (x :< xs) => x :< (y ': xs) where
  index = IS index


-- | Another list membership class, but this one requires uniqueness. That is,
-- @x :<! xs@ means there is only one instance of @x@ in @xs@. Again, there are
-- only two overlapping instances.
class (x :< xs) => (x :: k) :<! (xs :: [k])

instance {-# OVERLAPS #-} (Elem x xs ~ False) => x :<! (x ': xs)
instance {-# OVERLAPS #-} (x :<! xs) => x :<! (y ': xs)

-- | List membership type family.
type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[] = False
  Elem x (x ': xs) = True
  Elem x (y ': xs) = Elem x xs


-- | Convenience type family for consolidating multiple membership instances.
--
-- For example,
--
-- > (Reader r :< eff, Writer w :< eff, Base IO :< eff, Alt :< eff)
--
-- can be rewritten as
--
-- > ([Reader r, Writer w, Base IO, Alt] :<< eff)
--
type family (:<<) (xs :: [k]) (ys :: [k]) :: Constraint where
  '[] :<< ys = ()
  (x ': xs) :<< ys = (x :< ys, xs :<< ys)


-- | Inject an @f a@ into a @Union fs a@, which is possible so long as @f :< fs@.
--
-- This is similar to the injection
--
-- > Left :: a -> Either a b
--
-- For example, the following will typecheck:
--
-- @
-- inj (Just 5) :: Union [Maybe] Int
-- inj (Just 5) :: Union [Either Bool, Maybe] Int
-- inj (Just 5) :: Union [Maybe, Either Bool] Int
-- @
--
-- And the following will not:
--
-- @
-- inj (Just 5) :: Union [] Int            -- No instance for 'Maybe :< []'
-- inj (Just 5) :: Union [Either Bool] Int -- No instance for 'Maybe :< [Either Bool]'
-- @
--
inj :: (f :< fs) => f a -> Union fs a
inj f = Union f index

-- Project an @f a@ from a @Union fs a@, which is possible so long as @f :< fs@.
--
-- This is similar to the projection @fromLeft :: Either a b -> Maybe a@.
--
-- The @f :< fs@ constraint is necessary for the same reason that the type
-- signature @fromLeft :: Either a b -> Maybe c@ is nonsense - there is
-- obviously no way to project a @c@ from @Either a b@.
--
-- For example, the following will typecheck:
--
-- @
-- -- u :: Union [Maybe, Either Bool] Int
-- prj u :: Maybe (Maybe Int)
-- @
--
-- @
-- -- u :: Union [Maybe, Either Bool] Int
-- prj u :: Maybe (Either Bool Int)
-- @
--
-- And the following will not:
--
-- @
-- -- u :: Union [] Int
-- prj u :: Maybe (Maybe Int) -- No instance for 'Maybe :< []'
-- @
--
-- @
-- -- u :: Union [Either Bool] Int
-- prj u :: Maybe (Maybe Int) -- No instance for 'Maybe :< [Either Bool]'
-- @
--
prj :: forall a f fs. (f :< fs) => Union fs a -> Maybe (f a)
prj (Union f i) =
  case indexEq i (index :: Index f fs) of
    (:==:) -> Just f
    (:/=:) -> Nothing

decomp :: Union (f ': fs) x -> Either (Union fs x) (f x)
decomp (Union f IZ) = Right f
decomp (Union f (IS i)) = Left (Union f i)
