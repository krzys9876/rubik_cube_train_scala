package org.kr.cube.test

import org.kr.cube.{Cube3x3, FaceType, Moves3x3}
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

    Scenario("Create cube with given state"):
      Given("Initial state")
      val initialState = "UFFUFFUFFLLLLLLLLLBBDBBDBBDRRRRRRRRRBUUBUUBUUFDDFDDFDD"
      When("Cube is created")
      val cube = Cube3x3(initialState)
      Then("It is in a given state")
      assert(cube.state == initialState)

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

  Feature("Rotate face"):
    Scenario("F F'"):
      val cube1 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face1 = cube1.faces(FaceType.F)
      assert(face1.state === "FLBRFLBRF")
      val faceRotatedC = face1.rotated(Moves3x3.F.direction)
      assert(faceRotatedC.state === "BRFRFLFLB")
      val cube2 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face2 = cube2.faces(FaceType.F)
      val faceRotatedCC = face2.rotated(Moves3x3.F1.direction)
      assert(faceRotatedCC.state === "BLFLFRFRB")

    Scenario("L L'"):
      val cube1 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face1 = cube1.faces(FaceType.L)
      assert(face1.state === "LBRFLBRFL")
      val faceRotatedC = face1.rotated(Moves3x3.L.direction)
      assert(faceRotatedC.state === "RFLFLBLBR")
      val cube2 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face2 = cube2.faces(FaceType.L)
      val faceRotatedCC = face2.rotated(Moves3x3.L1.direction)
      assert(faceRotatedCC.state === "RBLBLFLFR")

    Scenario("B B'"):
      val cube1 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face1 = cube1.faces(FaceType.B)
      assert(face1.state === "BRFLBRFLB")
      val faceRotatedC = face1.rotated(Moves3x3.B.direction)
      assert(faceRotatedC.state === "FLBLBRBRF")
      val cube2 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face2 = cube2.faces(FaceType.B)
      val faceRotatedCC = face2.rotated(Moves3x3.B1.direction)
      assert(faceRotatedCC.state === "FRBRBLBLF")

    Scenario("R R'"):
      val cube1 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face1 = cube1.faces(FaceType.R)
      assert(face1.state === "RFLBRFLBR")
      val faceRotatedC = face1.rotated(Moves3x3.R.direction)
      assert(faceRotatedC.state === "LBRBRFRFL")
      val cube2 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face2 = cube2.faces(FaceType.R)
      val faceRotatedCC = face2.rotated(Moves3x3.R1.direction)
      assert(faceRotatedCC.state === "LFRFRBRBL")

    Scenario("U U'"):
      val cube1 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face1 = cube1.faces(FaceType.U)
      assert(face1.state === "FLBRFLBRF")
      val faceRotatedC = face1.rotated(Moves3x3.U.direction)
      assert(faceRotatedC.state === "BRFRFLFLB")
      val cube2 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face2 = cube2.faces(FaceType.U)
      val faceRotatedCC = face2.rotated(Moves3x3.U1.direction)
      assert(faceRotatedCC.state === "BLFLFRFRB")

    Scenario("D D'"):
      val cube1 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face1 = cube1.faces(FaceType.D)
      assert(face1.state === "LBRBRFLBR")
      val faceRotatedC = face1.rotated(Moves3x3.D.direction)
      assert(faceRotatedC.state === "LBLBRBRFR")
      val cube2 = Cube3x3("FLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRFLBRBRFLBR")
      val face2 = cube2.faces(FaceType.D)
      val faceRotatedCC = face2.rotated(Moves3x3.D1.direction)
      assert(faceRotatedCC.state === "RFRBRBLBL")

  Feature("Apply sequence of moves to solved cube"):
    Scenario("F F' L L' B B' R R' U U' D D'"):
      val moves = Vector(
        Moves3x3.F, Moves3x3.F1, Moves3x3.L, Moves3x3.L1, Moves3x3.B, Moves3x3.B1,
        Moves3x3.R, Moves3x3.R1, Moves3x3.U, Moves3x3.U1, Moves3x3.D, Moves3x3.D1)
      val cube = moves.foldLeft(Cube3x3.solved)((c, m) => m.applyToCube(c))
      assert(cube.isSolved)

    Scenario("4xF 4xL 4xB 4xR 4xU 4xD"):
      val moves = Vector.fill(4)(Moves3x3.F) ++ Vector.fill(4)(Moves3x3.L) ++ Vector.fill(4)(Moves3x3.B) ++
        Vector.fill(4)(Moves3x3.R) ++ Vector.fill(4)(Moves3x3.U) ++ Vector.fill(4)(Moves3x3.D)
      val cube = moves.foldLeft(Cube3x3.solved)((c, m) => m.applyToCube(c))
      assert(cube.isSolved)
    
    Scenario("4xF' 4xL' 4xB' 4xR' 4xU' 4xD'"):
      val moves = Vector.fill(4)(Moves3x3.F1) ++ Vector.fill(4)(Moves3x3.L1) ++ Vector.fill(4)(Moves3x3.B1) ++
        Vector.fill(4)(Moves3x3.R1) ++ Vector.fill(4)(Moves3x3.U1) ++ Vector.fill(4)(Moves3x3.D1)
      val cube = moves.foldLeft(Cube3x3.solved)((c, m) => m.applyToCube(c))
      assert(cube.isSolved)
    
    Scenario("F L B R U D"):
      val moves = Vector(Moves3x3.F, Moves3x3.L, Moves3x3.B, Moves3x3.R, Moves3x3.U, Moves3x3.D)
      val cube = moves.foldLeft(Cube3x3.solved)((c, m) => m.applyToCube(c))
      assert(cube.state === "UUUUFDBDDUFRULLRDDULLUBBFDDLBBRRRLFDBBRLURFFFLFFLDRBBR")
    
    Scenario("F' L' B' R' U' D'"):
      val moves = Vector(Moves3x3.F1, Moves3x3.L1, Moves3x3.B1, Moves3x3.R1, Moves3x3.U1, Moves3x3.D1)
      val cube = moves.foldLeft(Cube3x3.solved)((c, m) => m.applyToCube(c))
      assert(cube.state === "BUUDFUDDDRUUDLLDFRFUUDBBDLLLFURRRLBBBBRLURLFFFFFLDRBBR")
