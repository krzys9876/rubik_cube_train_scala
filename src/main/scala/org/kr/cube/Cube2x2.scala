package org.kr.cube

case class Cube2x2(faces: Map[Face2x2, Face]):
  lazy val isSolved: Boolean = state == Cube2x2.SOLVED_STATE

  private def f(face: Face2x2): String = faces(face).state
  private def s(face: Face2x2, index: Int): String = f(face).substring(index, index + 1)

  lazy val state: String =
    faces(Face2x2.F).state + faces(Face2x2.L).state + faces(Face2x2.B).state + faces(Face2x2.R).state +
      faces(Face2x2.U).state + faces(Face2x2.D).state

  def withFace(face: Face): Cube2x2 = Cube2x2(faces + (face.nominalFace -> face))

object Cube2x2:
  private val SOLVED_STATE: String = "FFFFLLLLBBBBRRRRUUUUDDDD"

  def solved: Cube2x2 = Cube2x2(SOLVED_STATE)

  def s(state: String, face: Face2x2, index: Int): String = state.substring(face.index + index, face.index + index + 1)
  def f(state: String, face: Face2x2): String = state.substring(face.index, face.index + 2*2)

  def apply(state: String): Cube2x2 =
    val faces: Map[Face2x2, Face] = Map(
      Face2x2.F -> Face(2, Axis.X, Axis.Y, Face2x2.F, state.substring(Face2x2.F.index, Face2x2.F.index + 2 * 2)),
      Face2x2.L -> Face(2, Axis.Zr, Axis.Y, Face2x2.L, state.substring(Face2x2.L.index, Face2x2.L.index + 2 * 2)),
      Face2x2.B -> Face(2, Axis.Xr, Axis.Y, Face2x2.B, state.substring(Face2x2.B.index, Face2x2.B.index + 2 * 2)),
      Face2x2.R -> Face(2, Axis.Z, Axis.Y, Face2x2.R, state.substring(Face2x2.R.index, Face2x2.R.index + 2 * 2)),
      Face2x2.U -> Face(2, Axis.X, Axis.Zr, Face2x2.U, state.substring(Face2x2.U.index, Face2x2.U.index + 2 * 2)),
      Face2x2.D -> Face(2, Axis.X, Axis.Z, Face2x2.D, state.substring(Face2x2.D.index, Face2x2.D.index + 2 * 2)))
    Cube2x2(faces)


sealed abstract class Move2x2(val symbol: String):
  def applyToCube(cube: Cube2x2): Cube2x2 = ???

  def edgeToEdge(origState: String, state: String,
                 faceFrom: Face2x2, indexFrom1: Int, indexFrom2: Int,
                 faceTo: Face2x2, indexTo1: Int, indexTo2: Int): String =
    replaceOne(origState,
      replaceOne(origState, state, faceFrom, indexFrom1, faceTo, indexTo1),
      faceFrom, indexFrom2, faceTo, indexTo2)

  private def replaceOne(origState: String, state: String,
                         faceFrom: Face2x2, indexFrom: Int, faceTo: Face2x2, indexTo: Int): String =
    state.substring(0, faceTo.index + indexTo) + Cube2x2.s(origState, faceFrom, indexFrom) +
      state.substring(faceTo.index + indexTo + 1)


object Moves2x2:
  case object F extends Move2x2("F"):
    override def applyToCube(cube: Cube2x2): Cube2x2 =
      val faceRotated = cube.faces(Face2x2.F).rotatedC
      val state = cube.withFace(faceRotated).state
      val stage1 = edgeToEdge(state, state, Face2x2.D, 0, 1, Face2x2.L, 1, 3)
      val stage2 = edgeToEdge(state, stage1, Face2x2.L, 1, 3, Face2x2.U, 2, 3)
      val stage3 = edgeToEdge(state, stage2, Face2x2.U, 2, 3, Face2x2.R, 0, 2)
      val stage4 = edgeToEdge(state, stage3, Face2x2.R, 2, 0, Face2x2.D, 0, 1)
      Cube2x2(stage4)

  case object F1 extends Move2x2("F'"):
    override def applyToCube(cube: Cube2x2): Cube2x2 =
      val faceRotated = cube.faces(Face2x2.F).rotatedCC
      val state = cube.withFace(faceRotated).state
      val stage1 = edgeToEdge(state, state, Face2x2.U, 2, 3, Face2x2.L, 1, 3)
      val stage2 = edgeToEdge(state, stage1, Face2x2.L, 1, 3, Face2x2.D, 0, 1)
      val stage3 = edgeToEdge(state, stage2, Face2x2.D, 0, 1, Face2x2.R, 0, 2)
      val stage4 = edgeToEdge(state, stage3, Face2x2.R, 2, 0, Face2x2.U, 2, 3)
      Cube2x2(stage4)

  case object L extends Move2x2("L")
  case object L1 extends Move2x2("L'")
  case object B extends Move2x2("B")
  case object B1 extends Move2x2("B'")
  case object R extends Move2x2("R")
  case object R1 extends Move2x2("R'")
  case object U extends Move2x2("U")
  case object U1 extends Move2x2("U'")
  case object D extends Move2x2("D")
  case object D1 extends Move2x2("D'")

sealed abstract class Face2x2(val symbol: String, val index: Int) {}

object Face2x2:
  case object F extends Face2x2("F", 0)
  case object L extends Face2x2("L", 4)
  case object B extends Face2x2("B", 8)
  case object R extends Face2x2("R", 12)
  case object U extends Face2x2("U", 16)
  case object D extends Face2x2("D", 20)

  def apply(symbol: String): Face2x2 = symbol match
      case "F" => F
      case "L" => L
      case "B" => B
      case "R" => R
      case "U" => U
      case "D" => D

abstract class Axis(val symbol: String, val reversed: Boolean) {}

object Axis:
  case object X extends Axis("X", false)
  case object Y extends Axis("Y", false)
  case object Z extends Axis("Z", false)
  case object Xr extends Axis("X", true)
  case object Zr extends Axis("Z", true)

case class TileCoords(r: Int, c: Int)

case class Tile(face: Face2x2, coords: TileCoords)

case class Face(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2, tiles: Vector[Tile]):
  def row(r: Int): Vector[Tile] = tiles.filter(_.coords.r == r)
  def col(c: Int): Vector[Tile] = tiles.filter(_.coords.c == c)
  lazy val state: String = tiles.map(_.face.symbol).mkString
  def rotatedC: Face =
    // flip coordinates clockwise
    val newTiles = tiles.map(t => tiles.find(_.coords == TileCoords(t.coords.c, size - 1 - t.coords.r)).get)
    new Face(size, axisV, axisH, nominalFace, newTiles)
  def rotatedCC: Face =
    // flip coordinates counterclockwise
    val newTiles = tiles.map(t => tiles.find(_.coords == TileCoords(size -1 - t.coords.c, t.coords.r)).get)
    new Face(size, axisV, axisH, nominalFace, newTiles)

object Face:
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2): Face =
    val tiles: Vector[Tile] = 0.until(size).flatMap(r => 0.until(size).map(c => Tile(nominalFace, TileCoords(r, c)))).toVector
    new Face(size, axisH, axisV, nominalFace, tiles)

  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2, state: String): Face =
    val tiles = 0.until(state.length).map(i =>
      val r = if(axisH.reversed) size - (i % size) -1 else i % size
      val c = if(axisV.reversed) size - (i / size) -1 else i / size
      Tile(Face2x2(state.substring(i, i + 1)), TileCoords(r, c))).toVector
    new Face(size, axisH, axisV, nominalFace, tiles)

