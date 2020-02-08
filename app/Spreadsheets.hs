{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Spreadsheets
    ( 
    )
  where

import LAoP.Matrix.Type
import GHC.Generics

data Student = Student1 | Student2 | Student3 | Student4
  deriving (Eq, Show, Enum, Bounded, Generic)

data Question = Question1 | Question2 | Question3 | Question4
  deriving (Eq, Show, Enum, Bounded, Generic)

data Results = Test | Exam | Final
  deriving (Eq, Show, Enum, Bounded, Generic)

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

xls :: Matrix Float Question One
    -> Matrix Float Question Student
    -> Matrix Float Student One
    -> Matrix Float (Either Question Results) (Either One Student)
xls w m t = junc (split w m) (split zeros r)
  where
    rExam = m `comp` tr w
    rTest = tr t
    rFinal = rTest `max` rExam
    rAux = junc rTest (junc rExam rFinal)
    r = tr (converter `comp` tr rAux)
    converter = junc test (junc exam final)
