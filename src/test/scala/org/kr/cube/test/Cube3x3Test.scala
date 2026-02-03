package org.kr.cube.test

import org.kr.cube.{Cube3x3, Moves3x3}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class Cube3x3Test extends AnyFeatureSpec with GivenWhenThen:
  Feature("Create cube"):
    Scenario("Create cube with solved state"):
      Given("No initial state")
      When("Cube is created")
      val cube = Cube3x3.solved
      println(cube.state)
      Then("It is is solved state")
      assert(cube.isSolved)

  Feature("Apply moves to solved cube"):
    Scenario("F"):
      assert(Moves3x3.F.applyToCube(Cube3x3.solved).state === "FFFFFFFFFLLDLLDLLDBBBBBBBBBURRURRURRUUUUUULLLRRRDDDDDD")
    Scenario("F'"):
      assert(Moves3x3.F1.applyToCube(Cube3x3.solved).state === "FFFFFFFFFLLULLULLUBBBBBBBBBDRRDRRDRRUUUUUURRRLLLDDDDDD")
    Scenario("L"):
      assert(Moves3x3.L.applyToCube(Cube3x3.solved).state === "UFFUFFUFFLLLLLLLLLBBDBBDBBDRRRRRRRRRBUUBUUBUUFDDFDDFDD")
    Scenario("L'"):
      assert(Moves3x3.L1.applyToCube(Cube3x3.solved).state === "DFFDFFDFFLLLLLLLLLBBUBBUBBURRRRRRRRRFUUFUUFUUBDDBDDBDD")
    Scenario("B"):
      assert(Moves3x3.B.applyToCube(Cube3x3.solved).state === "FFFFFFFFFULLULLULLBBBBBBBBBRRDRRDRRDRRRUUUUUUDDDDDDLLL")
    Scenario("B'"):
      assert(Moves3x3.B1.applyToCube(Cube3x3.solved).state === "FFFFFFFFFDLLDLLDLLBBBBBBBBBRRURRURRULLLUUUUUUDDDDDDRRR")
    Scenario("R"):
      assert(Moves3x3.R.applyToCube(Cube3x3.solved).state === "FFDFFDFFDLLLLLLLLLUBBUBBUBBRRRRRRRRRUUFUUFUUFDDBDDBDDB")
    Scenario("R'"):
      assert(Moves3x3.R1.applyToCube(Cube3x3.solved).state === "FFUFFUFFULLLLLLLLLDBBDBBDBBRRRRRRRRRUUBUUBUUBDDFDDFDDF")
    Scenario("U"):
      assert(Moves3x3.U.applyToCube(Cube3x3.solved).state === "RRRFFFFFFFFFLLLLLLLLLBBBBBBBBBRRRRRRUUUUUUUUUDDDDDDDDD")
    Scenario("U'"):
      assert(Moves3x3.U1.applyToCube(Cube3x3.solved).state === "LLLFFFFFFBBBLLLLLLRRRBBBBBBFFFRRRRRRUUUUUUUUUDDDDDDDDD")
    Scenario("D"):
      assert(Moves3x3.D.applyToCube(Cube3x3.solved).state === "FFFFFFLLLLLLLLLBBBBBBBBBRRRRRRRRRFFFUUUUUUUUUDDDDDDDDD")
    Scenario("D'"):
      assert(Moves3x3.D1.applyToCube(Cube3x3.solved).state === "FFFFFFRRRLLLLLLFFFBBBBBBLLLRRRRRRBBBUUUUUUUUUDDDDDDDDD")
    