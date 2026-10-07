{- |
Module     : LAoP.Matrix.Type
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : deprecated

Re-exports "LAoP.Matrix.Indexed", which replaces this module. It is kept so
imports keep resolving, but 0.2 code still needs changes: index types need a
'LAoP.Matrix.Indexed.MatIndex' instance. See the 0.3.0.0 changelog.
-}
module LAoP.Matrix.Type
  {-# DEPRECATED "Import LAoP.Matrix.Indexed. Index types now need MatIndex instances; see the 0.3.0.0 changelog." #-}
  (
    module LAoP.Matrix.Indexed,
  )
where

import           LAoP.Matrix.Indexed
