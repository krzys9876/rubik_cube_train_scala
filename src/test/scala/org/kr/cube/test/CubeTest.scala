package org.kr.cube.test

import org.kr.cube.Face2x2.F
import org.kr.cube.{Axis, Cube2x2, Face, Face2x2, Moves2x2}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class CubeTest extends AnyFeatureSpec with GivenWhenThen:
  Feature("Create cube"):
    Scenario("Create cube with solved state"):
      Given("No initial state")
      When("Cube is created")
      val cube = Cube2x2.solved
      Then("It is is solved state")
      assert(cube.isSolved)

    Scenario("Create cube with given state"):
      Given("Initial state")
      val initialState = "FFLLFFLLRRBBRRBBUUDDDDUU"
      When("Cube is created")
      val cube = Cube2x2(initialState)
      Then("It is in a given state")
      assert(cube.stateF == initialState)

  Feature("Apply moves to solved cube"):
    Scenario("F"):
      assert(Cube2x2.solved.move(Moves2x2.F).state === "FFFFLDLDBBBBURURUULLRRDD")
    Scenario("F1"):
      assert(Cube2x2.solved.move(Moves2x2.F1).state === "FFFFLULUBBBBDRDRUURRLLDD")

  Feature("internals"):
    Scenario("tiles"):
      val cube = Cube2x2("FFFFLDLDBBBBURURUULLRRDD")
      cube.faces.keys.toVector.sortBy(_.index).foreach(k => println(cube.faces(k)))

    Scenario("rotate face"):
      val face = Face(2, Axis.X, Axis.Y, Face2x2.F, "FLBR")
      println(face)
      val faceRotated = face.rotatedC
      println(faceRotated)
