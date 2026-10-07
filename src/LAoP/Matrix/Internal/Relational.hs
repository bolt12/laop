{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module     : LAoP.Matrix.Internal.Relational
Copyright  : (c) Armando Santos 2019-2026
Maintainer : armandoifsantos@gmail.com
Stability  : experimental

Relations as Boolean matrices: complement and the relational divisions.
-}
module LAoP.Matrix.Internal.Relational (
  Relation,
  negateM,
  divR,
  divL,
  divS,
) where

import           LAoP.Matrix.Internal.Boolean
import           LAoP.Matrix.Internal.Composition    (columnMajor, rowMajor,
                                                      rowsWithColumns)
import           LAoP.Matrix.Internal.Construction   (tr)
import           LAoP.Matrix.Internal.Dim            (Dim (..))
import           LAoP.Matrix.Internal.Elementwise    (emap, (.*.))
import           LAoP.Matrix.Internal.Representation

-- | Relation data type.
type Relation a b = Matrix Boolean a b

-- | Relational complement, element by element.
negateM :: Relation cols rows -> Relation cols rows
negateM = emap (fromBool . not . toBool)

{- | Relational right division. The element of @divR x y@ in row @c@ and
column @a@ holds when @x@ relates to @c@ every @b@ that @y@ relates to @a@: a
row of @x@ against a row of @y@, the conjunction over @b@ of implications.

laop 0.2 defined it with the four clauses of @comp@, conjunction in the place
of addition and implication in the place of multiplication, and they remain its
specification:

@
divR (One a)    (One b)    == One (fromBool (not (toBool b) || toBool a))
divR (Join a b) (Join c d) == divR a c .*. divR b d
divR (Fork a b) c          == Fork (divR a c) (divR b c)
divR c          (Fork a b) == Join (divR c a) (divR c b)
@

So it is the block recursion of @comp@, 'rowsWithColumns', with implication
in the place of the dot product. Its cost is that of @comp@, whatever the
layout of the two relations.
-}
divR :: Relation b c -> Relation b a -> Relation a c
divR x y = rowsWithColumns impliedBy (rowMajor x) (columnMajor (tr y))

-- A row of x against a column of the converse of y: whether every element of
-- the column implies the element of the row at the same position.
impliedBy :: Matrix Boolean b U -> Matrix Boolean U b -> Boolean
impliedBy (One a)      (One b)      = fromBool (not (toBool b) || toBool a)
impliedBy (Join a1 a2) (Fork b1 b2) = impliedBy a1 b1 * impliedBy a2 b2

-- | Matrix relational left division
divL :: Relation c b -> Relation a b -> Relation a c
divL x y = tr (divR (tr y) (tr x))

-- | Matrix relational symmetric division
divS :: Relation c a -> Relation b a -> Relation c b
divS s r = divL r s .*. divR (tr r) (tr s)
