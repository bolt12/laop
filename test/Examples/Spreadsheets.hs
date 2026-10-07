{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples.Spreadsheets (
  xls,
  maxPW,
  checks,
) where

import           GHC.Generics        (Generic)
import           LAoP.Category
import           LAoP.Matrix.Indexed
import           Prelude             hiding (id, (.))

data Student = Student1 | Student2 | Student3 | Student4
  deriving (Eq, Show, Generic)

instance MatIndex Student where
  type DimOf Student = GDimOf Student

data Question = Question1 | Question2 | Question3 | Question4
  deriving (Eq, Show, Generic)

instance MatIndex Question where
  type DimOf Question = GDimOf Question

data Results = Exam | Test | Final
  deriving (Eq, Show, Generic)

instance MatIndex Results where
  type DimOf Results = GDimOf Results

test :: Matrix Float One Results
test = point Test

exam :: Matrix Float One Results
exam = point Exam

final :: Matrix Float One Results
final = point Final

m :: Matrix Float Question Student
m = fromLists [[95, 90, 100, 40], [20, 90, 90, 0], [30, 20, 95, 0], [50, 80, 100, 30]]

w :: Matrix Float Question One
w = fromLists [[0.2, 0.3, 0.2, 0.3]]

xls ::
  Matrix Float Student One ->
  Matrix Float (Either Question Results) (Either One Student)
xls t = join (fork w m) (fork zeros r)
  where
    rExam = m . tr w
    rTest = tr t
    rFinal = rTest `maxPW` rExam
    r = (rExam . tr exam) .+. (rTest . tr test) .+. (rFinal . tr final)

maxPW :: (Ord e) => Matrix e a b -> Matrix e a b -> Matrix e a b
maxPW = zipWithM max

-- Checks

-- With test marks 60, 50, 70 and 40, the weighted exam marks are 78, 49, 31
-- and 63, and the final mark is the larger of the two.
checks :: [(String, Bool)]
checks =
  [ ( "the sheet has the weights on top and exam, test and final beside the answers"
    , and (zipWith close (concat (toLists (xls testMarks))) (concat expected))
    )
  ]
  where
    testMarks = fromLists [[60, 50, 70, 40]]
    expected =
      [ [0.2, 0.3, 0.2, 0.3, 0, 0, 0]
      , [95, 90, 100, 40, 78, 60, 78]
      , [20, 90, 90, 0, 49, 50, 50]
      , [30, 20, 95, 0, 31, 70, 70]
      , [50, 80, 100, 30, 63, 40, 63]
      ]
    close x y = abs (x - y) < 1e-4
