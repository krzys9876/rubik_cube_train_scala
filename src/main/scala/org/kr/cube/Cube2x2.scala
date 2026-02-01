package org.kr.cube

case class Cube2x2(faces: Map[Face2x2, Face]):
  lazy val isSolved: Boolean = state == Cube2x2.SOLVED_STATE

  private def f(face: Face2x2): String = faces(face).state
  private def s(face: Face2x2, index: Int): String = f(face).substring(index, index + 1)

  lazy val state: String =
    faces(Face2x2.F).state + faces(Face2x2.L).state + faces(Face2x2.B).state + faces(Face2x2.R).state +
      faces(Face2x2.U).state + faces(Face2x2.D).state

  def withFace(face: Face): Cube2x2 = Cube2x2(faces + (face.nominalFace -> face))

  def slice(axis: Axis, i: Int): Slice =
    val edges = Face2x2.faceOrder(axis).map(face => faces(face).edge(axis, i))
    Slice(edges)

  def withSliceRotated(slice: Slice): Cube2x2 =
    slice.edgePairs.foldLeft(this)({ case (c, (eFrom, eTo)) =>
      //NOTE: we must take original faces as we replace all faces, and we effectively overwrite the first with the las
      val faceTo = this.faces(eTo.nominalFace)
      val faceReplaced = faceTo.withEdge(eTo, eFrom)
      c.withFace(faceReplaced)
    })


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
      val slice = cube.slice(Axis.Z, 0)
      cube.withFace(faceRotated).withSliceRotated(slice)

  case object F1 extends Move2x2("F'"):
    override def applyToCube(cube: Cube2x2): Cube2x2 =
      val faceRotated = cube.faces(Face2x2.F).rotatedCC
      val slice = cube.slice(Axis.Zr, 0)
      cube.withFace(faceRotated).withSliceRotated(slice)

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

  private val faceOrderX: Vector[Face2x2] = Vector(Face2x2.F, Face2x2.U, Face2x2.B, Face2x2.D)
  private val faceOrderY: Vector[Face2x2] = Vector(Face2x2.F, Face2x2.L, Face2x2.B, Face2x2.R)
  private val faceOrderZ: Vector[Face2x2] = Vector(Face2x2.L, Face2x2.U, Face2x2.R, Face2x2.D)

  val faceOrder: Map[Axis, Vector[Face2x2]] =
    Map(Axis.X -> faceOrderX, Axis.Y -> faceOrderY, Axis.Z -> faceOrderZ,
      Axis.Xr -> faceOrderX.reverse, Axis.Yr -> faceOrderY.reverse, Axis.Zr -> faceOrderZ.reverse)


abstract class Axis(val symbol: String, val reversed: Boolean)

// NOTE: Y axis is screen-like: growing down, not up
object Axis:
  case object X extends Axis("X", false)
  case object Y extends Axis("Y", false)
  case object Z extends Axis("Z", false)
  case object Xr extends Axis("X", true)
  case object Yr extends Axis("Y", true)
  case object Zr extends Axis("Z", true)

case class TileCoords(r: Int, c: Int)

case class Tile(face: Face2x2, coords: TileCoords)

case class Face(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2, tiles: Vector[Tile]):
  private def row(r: Int): Vector[Tile] = tiles.filter(_.coords.r == r)
  private def col(c: Int): Vector[Tile] = tiles.filter(_.coords.c == c)
  lazy val state: String = tiles.map(_.face.symbol).mkString
  def rotatedC: Face =
    // flip coordinates clockwise
    val newTiles = tiles.map(t => tiles.find(_.coords == TileCoords(t.coords.c, size - 1 - t.coords.r)).get)
    new Face(size, axisV, axisH, nominalFace, newTiles)
  def rotatedCC: Face =
    // flip coordinates counterclockwise
    val newTiles = tiles.map(t => tiles.find(_.coords == TileCoords(size -1 - t.coords.c, t.coords.r)).get)
    new Face(size, axisV, axisH, nominalFace, newTiles)
  def edge(axis: Axis, i: Int): Edge =
    axis.symbol match
      case axisH.symbol => Edge(nominalFace, row(i))
      case axisV.symbol => Edge(nominalFace, col(i))
  def withEdge(currentEdge: Edge, newEdge: Edge): Face =
    val newTiles = currentEdge.tiles.indices.foldLeft(tiles)((t, i) =>
      val tileIndex = t.indexWhere(_.coords == currentEdge.tiles(i).coords)
      t.updated(tileIndex, Tile(newEdge.tiles(i).face, currentEdge.tiles(i).coords)))
    copy(tiles = newTiles)

object Face:
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2): Face =
    val tiles: Vector[Tile] = 0.until(size).flatMap(r => 0.until(size).map(c => Tile(nominalFace, TileCoords(r, c)))).toVector
    new Face(size, axisH, axisV, nominalFace, tiles)

  // Faces are always placed in order: ABCD:
  // AB
  // CD
  // But they may have reversed coords. This simplifies textual state
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2, state: String): Face =
    val tiles = 0.until(state.length).map(i =>
      val r = if(axisH.reversed) size - (i % size) -1 else i % size
      val c = if(axisV.reversed) size - (i / size) -1 else i / size
      Tile(Face2x2(state.substring(i, i + 1)), TileCoords(r, c))).toVector
    new Face(size, axisH, axisV, nominalFace, tiles)

case class Edge(nominalFace: Face2x2, tiles: Vector[Tile])

case class Slice(edges: Vector[Edge]):
  val edgePairs: Vector[(Edge, Edge)] = edges.zip(edges.drop(1).appended(edges.head))