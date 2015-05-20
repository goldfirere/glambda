{-# LANGUAGE GADTs #-}

module Ex1 where

-- | A very simple newtype wrapper
newtype Wrap a = Wrap a

-- | A type-indexed representation of a type
data STy ty where
  SIntTy   :: STy Int
  SBoolTy  :: STy Bool
  SMaybeTy :: STy a -> STy (Maybe a)

-- | Produce a "zero" of that type
zero :: STy ty -> ty
zero SIntTy       = 0
zero SBoolTy      = False
zero (SMaybeTy _) = Nothing
