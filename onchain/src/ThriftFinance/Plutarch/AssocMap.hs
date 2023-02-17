-- This module should be made redundant by plutarch-extra utilities eventually.
module ThriftFinance.Plutarch.AssocMap (
  pwithoutKey,
  phasAnyOf,
  pinsertWith,
) where

import Plutarch.Api.V1 (KeyGuarantees (Sorted), PMap (PMap))
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Unsafe (punsafeDowncast)

-- | Check if the 'PMap' has any of the given keys.
phasAnyOf :: PIsListLike list (PAsData k) => Term s (list (PAsData k) :--> PMap any k v :--> PBool)
phasAnyOf = phoistAcyclic $ plam $ \ks m -> pany # plam (\p -> pelem # (pfstBuiltin # p) # ks) #$ pto m

-- | Remove the given key from a map.
pwithoutKey :: Term s (PAsData k :--> PMap any k v :--> PMap any k v)
pwithoutKey = phoistAcyclic $
  plam $ \k m ->
    pcon $
      PMap $
        precList
          ( \self x xs ->
              pif
                (pfstBuiltin # x #== k)
                xs
                (pcons # x #$ self # xs)
          )
          (const pnil)
          #$ pto m

-- Insert a new key/value pair into the map. If a pair with the given key is
-- already present, the value is passed so it can be modified.
pinsertWith :: (POrd k, PIsData k, PIsData v) => Term s ((v :--> v :--> v) :--> k :--> v :--> PMap 'Sorted k v :--> PMap 'Sorted k v)
pinsertWith = phoistAcyclic $
  plam $ \combine key val ->
    let f = plam $ \maybeVal m -> pmatch maybeVal $ \case
          PJust val' ->
            pcons
              # (ppairDataBuiltin # pdata key #$ pdata $ combine # val' # val)
              # m
          PNothing ->
            pcons
              # (ppairDataBuiltin # pdata key # pdata val)
              # m
     in rebuildAtKeyWithValue # f # key

{- | Rebuild the map at the given key. The handler function obtains the
 value of the pair if already present.
-}
rebuildAtKeyWithValue ::
  (POrd k, PIsData k, PIsData v) =>
  Term
    s
    ( ( PMaybe v
          :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
          :--> PBuiltinList (PBuiltinPair (PAsData k) (PAsData v))
      )
        :--> k
        :--> PMap g k v
        :--> PMap g k v
    )
rebuildAtKeyWithValue = phoistAcyclic $
  plam $ \handler key map ->
    punsafeDowncast $
      precList
        ( \self x xs ->
            plet (pfromData $ pfstBuiltin # x) $ \k ->
              plet (pfromData $ psndBuiltin # x) $ \v ->
                plam $ \prefix ->
                  pif
                    (k #< key)
                    (self # xs #$ plam $ \suffix -> prefix #$ pcons # x # suffix)
                    ( pif
                        (k #== key)
                        (prefix #$ handler # pcon (PJust v) # xs)
                        (prefix #$ handler # pcon PNothing #$ pcons # x # xs)
                    )
        )
        (\_ -> plam (#$ handler # pcon PNothing # pnil))
        # pto map
        # plam id
