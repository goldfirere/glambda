{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ex2 where

-- | An 'HList' is a heterogeneous list, indexed by a type-level list
-- of types.
data HList tys where
  Nil  :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)
infixr 5 :>

-- | @Elem xs x@ is evidence that @x@ is in the list @xs@.
-- @EZ :: Elem xs x@ is evidence that @x@ is the first element of @xs@.
-- @ES ev :: Elem xs x@ is evidence that @x@ is one position later in
-- @xs@ than is indicated in @ev@
data Elem list elt where
  EZ :: Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

-- | Get retrieves an item out of a heterogeneous list
get :: Elem tys ty -> HList tys -> ty
get = undefined
