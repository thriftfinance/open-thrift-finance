module ThriftFinance.Plutarch.List (pelemBy, psingleMatch, psingleElem) where

-- | / O(n) /. Check if element is in the list
pelemBy :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
pelemBy =
  phoistAcyclic $
    plam $ \needlef ->
      precList
        (\self x xs -> pif (needlef # x) (pcon PTrue) (self # xs))
        (\_self -> pcon PFalse)

{- | This filters a list with given predicate, ensures the result is a singleton
 list, and extracts the result.  It's like doing `isSingleton . filter` and then
 extracting `head`.
-}
psingleMatch :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PMaybe a)
psingleMatch = phoistAcyclic $
  plam $ \predc ->
    ( pfix #$ plam $ \self acc l ->
        pelimList
          ( \x xs ->
              pif
                (predc # x)
                ( pmatch acc $ \case
                    PJust _ -> pnothing
                    PNothing -> self # pjust x # xs
                )
                $ self # acc # xs
          )
          acc
          l
    )
      # pnothing
  where
    pnothing = pcon PNothing
    pjust = pcon . PJust

-- | Makes sure only one element of a list satisfies given predicate.
psingleElem :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
psingleElem = phoistAcyclic $
  plam $ \predc l -> pmatch (psingleMatch # predc # l) $ \case
    PNothing -> pconstant False
    PJust _ -> pconstant True
