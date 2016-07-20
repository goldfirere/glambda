{-# LANGUAGE GADTs #-}

module Ex1 where

-- | A very simple newtype wrapper
newtype Wrap a = Wrap a

-- | A type-indexed representation of a type
data STy ty where
  SIntTy   :: STy Int
  SBoolTy  :: STy Bool
  SMaybeTy :: STy a -> STy (Maybe a)
  SListTy  :: STy a -> STy [a]
  SWrapTy  :: STy a -> STy (Wrap a)
  SUnitTy  :: STy ()
  SArrowTy :: STy arg -> STy res -> STy (arg -> res)

-- | Produce a "zero" of that type
zero :: STy ty -> ty
zero SIntTy           = 0
zero SBoolTy          = False
zero (SMaybeTy _)     = Nothing
zero (SListTy _)      = []
zero (SWrapTy t)      = Wrap (zero t)
zero SUnitTy          = ()
zero (SArrowTy _ res) = \_ -> zero res
