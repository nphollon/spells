module Test.Suite (all) where

import ElmTest.Test exposing (Test, suite, test)
import ElmTest.Assertion exposing (assert)

all : Test
all =
  suite "Test Suite"
          [ test "dummy" (assert True)
          ]
