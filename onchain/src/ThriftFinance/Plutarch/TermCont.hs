module ThriftFinance.Plutarch.TermCont (
  pconstantC,
  pifC,
  pverifyDataC,
) where

import Plutarch.Lift (PLifted)

pconstantC :: forall (p :: PType) (s :: S). PLift p => PLifted p -> TermCont s (Term s p)
pconstantC = pure . pconstant

pifC :: forall (s :: S) (a :: PType). Term s PBool -> Term s a -> Term s a -> TermCont s (Term s a)
pifC cond branch1 branch2 = pure $ pif cond branch1 branch2

-- | Validate the structure of 'PData', parsing it into a 'PAsData a' for given 'a'.
pverifyDataC :: forall a r s. PTryFrom PData a => Term s PData -> TermCont @r s (Term s a)
pverifyDataC = fmap fst . tcont . ptryFrom
