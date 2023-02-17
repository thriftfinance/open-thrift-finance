module ThriftFinance.Plutarch.Bool (
  pall',
) where

import Data.List (foldl')

{- | Convenient helper for joining many conditions at the Term level.
 pall' [c1, c2, ...] = c1 #&& c2 #&& ...
-}
pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldl' (#&&) (pconstant True)
