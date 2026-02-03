package org.kr.cube.test

import org.kr.cube.{Axis, Cube2x2, Face, FaceType, Moves2x2, Tile}
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.annotation.tailrec

class Cube2x2Test extends AnyFeatureSpec with GivenWhenThen:
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
    Scenario("F'"):
      assert(Moves2x2.F1.applyToCube(Cube2x2.solved).state === "FFFFLULUBBBBDRDRUURRLLDD")
    Scenario("L"):
      assert(Moves2x2.L.applyToCube(Cube2x2.solved).state === "UFUFLLLLBDBDRRRRBUBUFDFD")
    Scenario("L'"):
      assert(Moves2x2.L1.applyToCube(Cube2x2.solved).state === "DFDFLLLLBUBURRRRFUFUBDBD")
    Scenario("B"):
      assert(Moves2x2.B.applyToCube(Cube2x2.solved).state === "FFFFULULBBBBRDRDRRUUDDLL")
    Scenario("B'"):
      assert(Moves2x2.B1.applyToCube(Cube2x2.solved).state === "FFFFDLDLBBBBRURULLUUDDRR")
    Scenario("R"):
      assert(Moves2x2.R.applyToCube(Cube2x2.solved).state === "FDFDLLLLUBUBRRRRUFUFDBDB")
    Scenario("R'"):
      assert(Moves2x2.R1.applyToCube(Cube2x2.solved).state === "FUFULLLLDBDBRRRRUBUBDFDF")
    Scenario("U"):
      assert(Moves2x2.U.applyToCube(Cube2x2.solved).state === "RRFFFFLLLLBBBBRRUUUUDDDD")
    Scenario("U'"):
      assert(Moves2x2.U1.applyToCube(Cube2x2.solved).state === "LLFFBBLLRRBBFFRRUUUUDDDD")
    Scenario("D"):
      assert(Moves2x2.D.applyToCube(Cube2x2.solved).state === "FFLLLLBBBBRRRRFFUUUUDDDD")
    Scenario("D'"):
      assert(Moves2x2.D1.applyToCube(Cube2x2.solved).state === "FFRRLLFFBBLLRRBBUUUUDDDD")

  Feature("Rotate face"):
    Scenario("F F'"):
      val cube1 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face1 = cube1.faces(FaceType.F)
      assert(face1.state === "FLBR")
      val faceRotatedC = face1.rotated(Moves2x2.F.direction)
      assert(faceRotatedC.state === "BFRL")
      val cube2 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face2 = cube2.faces(FaceType.F)
      val faceRotatedCC = face2.rotated(Moves2x2.F1.direction)
      assert(faceRotatedCC.state === "LRFB")

    Scenario("L L'"):
      val cube1 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face1 = cube1.faces(FaceType.L)
      assert(face1.state === "FLBR")
      val faceRotatedC = face1.rotated(Moves2x2.L.direction)
      assert(faceRotatedC.state === "BFRL")
      val cube2 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face2 = cube2.faces(FaceType.L)
      val faceRotatedCC = face2.rotated(Moves2x2.L1.direction)
      assert(faceRotatedCC.state === "LRFB")

    Scenario("B B'"):
      val cube1 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face1 = cube1.faces(FaceType.B)
      assert(face1.state === "FLBR")
      val faceRotatedC = face1.rotated(Moves2x2.B.direction)
      assert(faceRotatedC.state === "BFRL")
      val cube2 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face2 = cube2.faces(FaceType.B)
      val faceRotatedCC = face2.rotated(Moves2x2.B1.direction)
      assert(faceRotatedCC.state === "LRFB")

    Scenario("R R'"):
      val cube1 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face1 = cube1.faces(FaceType.R)
      assert(face1.state === "FLBR")
      val faceRotatedC = face1.rotated(Moves2x2.R.direction)
      assert(faceRotatedC.state === "BFRL")
      val cube2 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face2 = cube2.faces(FaceType.R)
      val faceRotatedCC = face2.rotated(Moves2x2.R1.direction)
      assert(faceRotatedCC.state === "LRFB")

    Scenario("U U'"):
      val cube1 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face1 = cube1.faces(FaceType.U)
      assert(face1.state === "FLBR")
      val faceRotatedC = face1.rotated(Moves2x2.U.direction)
      assert(faceRotatedC.state === "BFRL")
      val cube2 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face2 = cube2.faces(FaceType.U)
      val faceRotatedCC = face2.rotated(Moves2x2.U1.direction)
      assert(faceRotatedCC.state === "LRFB")

    Scenario("D D'"):
      val cube1 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face1 = cube1.faces(FaceType.D)
      assert(face1.state === "FLBR")
      val faceRotatedC = face1.rotated(Moves2x2.D.direction)
      assert(faceRotatedC.state === "BFRL")
      val cube2 = Cube2x2("FLBRFLBRFLBRFLBRFLBRFLBR")
      val face2 = cube2.faces(FaceType.D)
      val faceRotatedCC = face2.rotated(Moves2x2.D1.direction)
      assert(faceRotatedCC.state === "LRFB")


  Feature("Apply sequence of moves to solved cube"):
    Scenario("F F' L L' B B' R R' U U' D D'"):
      val moves = Vector(
        Moves2x2.F, Moves2x2.F1, Moves2x2.L, Moves2x2.L1, Moves2x2.B, Moves2x2.B1,
        Moves2x2.R, Moves2x2.R1, Moves2x2.U, Moves2x2.U1, Moves2x2.D, Moves2x2.D1)
      val cube = moves.foldLeft(Cube2x2.solved)((c, m) => m.applyToCube(c))
      assert(cube.isSolved)

    Scenario("4xF 4xL 4xB 4xR 4xU 4xD"):
      val moves = Vector.fill(4)(Moves2x2.F) ++ Vector.fill(4)(Moves2x2.L) ++ Vector.fill(4)(Moves2x2.B) ++
        Vector.fill(4)(Moves2x2.R) ++ Vector.fill(4)(Moves2x2.U) ++ Vector.fill(4)(Moves2x2.D)
      val cube = moves.foldLeft(Cube2x2.solved)((c, m) => m.applyToCube(c))
      assert(cube.isSolved)

    Scenario("4xF' 4xL' 4xB' 4xR' 4xU' 4xD'"):
      val moves = Vector.fill(4)(Moves2x2.F1) ++ Vector.fill(4)(Moves2x2.L1) ++ Vector.fill(4)(Moves2x2.B1) ++
        Vector.fill(4)(Moves2x2.R1) ++ Vector.fill(4)(Moves2x2.U1) ++ Vector.fill(4)(Moves2x2.D1)
      val cube = moves.foldLeft(Cube2x2.solved)((c, m) => m.applyToCube(c))
      assert(cube.isSolved)

    Scenario("F L B R U D"):
      val moves = Vector(Moves2x2.F, Moves2x2.L, Moves2x2.B, Moves2x2.R, Moves2x2.U, Moves2x2.D)
      val cube = moves.foldLeft(Cube2x2.solved)((c, m) => m.applyToCube(c))
      assert(cube.state === "UUBDURRDULFDLBLDBRFFLFBR")

    Scenario("F' L' B' R' U' D'"):
      val moves = Vector(Moves2x2.F1, Moves2x2.L1, Moves2x2.B1, Moves2x2.R1, Moves2x2.U1, Moves2x2.D1)
      val cube = moves.foldLeft(Cube2x2.solved)((c, m) => m.applyToCube(c))
      assert(cube.state === "BUDDRUDRFUDLLULBBRLFFFBR")

  Feature("Mask tiles"):
    Scenario("F"):
      val cube = Cube2x2.solvedWithMask((f: Face, t: Tile) => f.nominalFace == FaceType.U || (f.axisV.symbol == "Y" && t.coords.r == 0))
      assert(cube.maskedState === "..FF..LL..BB..RR....DDDD")
      val after = Moves2x2.F.applyToCube(cube)
      assert(after.state === "FFFFLDLDBBBBURURUULLRRDD")
      assert(after.maskedState === "F.F..DLD..BB...R..L.R.DD")

  Feature("internals"):
    Scenario("tiles"):
      val cube = Cube2x2("FFFFLDLDBBBBURURUULLRRDD")
      cube.faces.keys.toVector.sortBy(k => Cube2x2.faceStateIndex(k)).foreach(k => println(cube.faces(k)))

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

/*    Scenario("random moves (white face"):
      (0 until 10000).foreach(i =>
        val(cube, res, log) = trySolve(Cube2x2.solved, Cube2x2.solved.state, "000000000000000000001111", 100)
        if(res) println(f"$i ${cube.state} ${log.length} ${log.mkString(" ")}")
      )*/
  
  Feature("Compare masked state"):
    Scenario("Compare single face"):
      val cube1 = Cube2x2.solved
      val cube2 = Cube2x2("FFFFLDLDBBBBURURUULLRRDD")
      val mask = "111100000000000000000000"
      assert(Cube2x2.maskedEquals(cube1.state, cube2.state, mask))
