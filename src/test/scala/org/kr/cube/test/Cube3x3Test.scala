package org.kr.cube.test

import org.kr.cube.Cube3x3
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