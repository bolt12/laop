{-# LANGUAGE TypeFamilies #-}

{- |
Module     : LAoP.Category
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

The identity and composition that functions, matrices and relations share.
Hide Prelude's versions to use these:

@
import           LAoP.Category
import           Prelude       hiding (id, (.))
@

With them, @f . g@ composes two matrices as it composes two functions, and the
laws of a category hold for both.
-}
module LAoP.Category (Category (..)) where

import           Data.Kind (Constraint, Type)
import           Prelude   hiding (id, (.))
import qualified Prelude

infixr 9 .

{- | A category whose objects can be constrained. For matrices 'Object' says
which types can index a dimension; for functions there is no constraint.

@.@ is right-associative, like 'Prelude..', so a chain @f . g . v@ applied to a
vector @v@ multiplies matrix by vector twice rather than first forming @f . g@.
-}
class Category (k :: j -> j -> Type) where
  type Object k (o :: j) :: Constraint
  type Object k o = ()
  id :: (Object k a) => k a a
  (.) :: k b c -> k a b -> k a c

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)
