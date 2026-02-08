{- | IxSet utilities for multi-item index operations.

These complement @ixset-typed@'s single-item 'Ix.updateIx'/'Ix.deleteIx'
for cases where multiple items share the same index value (e.g., all tasks
from one source file sharing the same 'FilePath' index).
-}
module Ob.Vault.Ix (
  updateIxMulti,
  deleteIxMulti,
)
where

import Data.IxSet.Typed ((@=))
import Data.IxSet.Typed qualified as Ix

{- | Like 'Ix.updateIx', but works for multiple items sharing an index.

Deletes all existing items matching the index, then unions in the new set.
-}
updateIxMulti ::
  (Ix.IsIndexOf ix ixs, Ix.Indexable ixs a) =>
  ix ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a
updateIxMulti r new rels =
  let old = rels @= r
      deleteMany = foldr Ix.delete
   in new `Ix.union` (rels `deleteMany` old)

{- | Like 'Ix.deleteIx', but works for multiple items sharing an index.

Removes all items matching the given index.
-}
deleteIxMulti ::
  (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs) =>
  ix ->
  Ix.IxSet ixs a ->
  Ix.IxSet ixs a
deleteIxMulti r rels =
  let candidates = Ix.toList $ Ix.getEQ r rels
   in flipfoldl' Ix.delete rels candidates
