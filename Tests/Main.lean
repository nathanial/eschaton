import Crucible
import Eschaton

open Crucible

testSuite "Eschaton.Basic"

test "basic setup works" := do
  shouldSatisfy true "project is set up"

#generate_tests

def main : IO UInt32 := runAllSuites
