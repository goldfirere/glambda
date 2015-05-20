-- Very simple GADT example
-- Copyright (c) 2015 Richard Eisenberg

{-# LANGUAGE GADTs #-}

module G where

data G a where
  MkGInt  :: G Int
  MkGBool :: G Bool

frob :: G a -> a
frob MkGInt  = 5
frob MkGBool = False
