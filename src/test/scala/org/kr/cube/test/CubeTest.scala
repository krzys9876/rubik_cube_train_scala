package org.kr.cube.test

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
      assert(cube.state == initialState)

  Feature("Apply moves to solved cube"):
    Scenario("F"):
      assert(Moves2x2.F.applyToCube(Cube2x2.solved).state === "FFFFLDLDBBBBURURUULLRRDD")
    Scenario("F1"):
      assert(Moves2x2.F1.applyToCube(Cube2x2.solved).state === "FFFFLULUBBBBDRDRUURRLLDD")

  Feature("internals"):
    Scenario("tiles"):
      val cube = Cube2x2("FFFFLDLDBBBBURURUULLRRDD")
      cube.faces.keys.toVector.sortBy(_.index).foreach(k => println(cube.faces(k)))

    Scenario("rotate face"):
      val face = Face(2, Axis.X, Axis.Y, Face2x2.F, "FLBR")
      println(face)
      val faceRotated = face.rotatedC
      println(faceRotated)

    Scenario("slices"):
      val cube = Cube2x2.solved
      val sliceY0 = cube.slice(Axis.Y,0)
      println("Y0")
      println(sliceY0.edges.mkString("\n"))
      val sliceY1 = cube.slice(Axis.Y, 1)
      println("Y1")
      println(sliceY1.edges.mkString("\n"))
      val sliceX0 = cube.slice(Axis.X, 0)
      println("X0")
      println(sliceX0.edges.mkString("\n"))
      val sliceX1 = cube.slice(Axis.X, 1)
      println("X1")
      println(sliceX1.edges.mkString("\n"))
      val sliceZ0 = cube.slice(Axis.Z, 0)
      println("Z0")
      println(sliceZ0.edges.mkString("\n"))
      val sliceZ1 = cube.slice(Axis.Z, 1)
      println("Z1")
      println(sliceZ1.edges.mkString("\n"))
      val sliceZ1r = cube.slice(Axis.Zr, 1)
      println("Z1r")
      println(sliceZ1r.edges.mkString("\n"))
      println(sliceZ1r.edgePairs.mkString("\n"))
      println("----------------")
      println(sliceZ0.edgePairs.mkString("\n"))
      val rotatedCube=sliceZ0.edgePairs.foldLeft(cube)({case(c,(eFrom, eTo)) =>
          val faceFrom = cube.faces(eFrom.nominalFace)
          val faceTo = cube.faces(eTo.nominalFace)
          println(faceFrom)
          println(faceTo)
          val faceReplaced = faceTo.withEdge(eTo, eFrom)
          c.withFace(faceReplaced)
      })
      println(rotatedCube.state)

/*    Scenario("rotate slice"):
      val cube = Cube2x2.solved
      val sliceZ0 = cube.slice(Axis.Z, 0)
      sliceZ0.edges.foldLeft(cube)((c,e) => Moves2x2.R.applyToCube(c))*/