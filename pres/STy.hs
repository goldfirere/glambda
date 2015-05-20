{-# LANGUAGE GADTs #-}

module STy where

data STy ty where
  SIntTy :: STy Int
  SBoolTy :: STy Bool
  SMaybeTy :: STy a -> STy (Maybe a)
  SArr :: STy a -> STy b -> STy (a -> b)

zero :: STy ty -> ty
zero SIntTy = 0
zero SBoolTy = False
zero (SMaybeTy _) = Nothing
zero (SArr _ res) = const (zero res)

eqSTy :: STy ty -> STy ty -> Bool
eqSTy SIntTy SIntTy = True
{-
eqSTy SBoolTy SBoolTy = True
eqSTy (SMaybeTy t1) (SMaybeTy t2) = t1 `eqSTy` t2
eqSTy (t1 `SArr` t2) (t3 `SArr` t4) = t1 `eqSTy` t3 && t2 `eqSTy` t4
-}
