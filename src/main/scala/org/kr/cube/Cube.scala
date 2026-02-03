package org.kr.cube

import scala.collection.mutable

abstract class Cube:
  val faces: mutable.Map[FaceType, Face]
  def isSolved: Boolean

  def state: String =
    faces(FaceType.F).state + faces(FaceType.L).state + faces(FaceType.B).state + faces(FaceType.R).state +
      faces(FaceType.U).state + faces(FaceType.D).state

  def maskedState: String =
    new StringBuilder().append(faces(FaceType.F).maskedState)
      .append(faces(FaceType.L).maskedState)
      .append(faces(FaceType.B).maskedState)
      .append(faces(FaceType.R).maskedState)
      .append(faces(FaceType.U).maskedState)
      .append(faces(FaceType.D).maskedState).result()

  private def withFace(face: Face): Cube =
    faces.update(face.nominalFace, face)
    this

  def slice(axis: Axis, i: Int): Slice =
    val edges = FaceType.faceOrder(axis).map({ case (face, sort) => faces(face).edge(axis, i, sort) })
    Slice(edges)

  def withSliceRotated(faceRotated: Face, slice: Slice): Cube =
    val newFaces = slice.edgePairs.map({ case (eFrom, eTo) =>
      //NOTE: we must take original faces as we replace all faces, and we effectively overwrite the first with the last
      eTo.nominalFace -> this.faces(eTo.nominalFace).withEdge(eTo, eFrom)
    })
    faces += (faceRotated.nominalFace -> faceRotated)
    faces ++= newFaces
    this

  def applyMask(mask: (Face, Tile) => Boolean): Cube =
    faces.values.foldLeft(this)((c, f) =>
      c.withFace(f.copy(tiles = f.tiles.map(t => t.copy(masked = mask(f, t))))))

  def tileSymbol(faceType: FaceType, index: Int): String =
    faces(faceType).tiles(index).face.symbol match {
      case "F" => "\u001B[34mF\u001B[0m"
      case "L" => "\u001B[38;2;255;165;0mL\u001B[0m"
      case "B" => "\u001B[32mB\u001B[0m"
      case "R" => "\u001B[31mR\u001B[0m"
      case "U" => "\u001B[33mU\u001B[0m"
      case "D" => "\u001B[0mD"
    }

  def printableState: String


case class Cube2x2(override val faces: mutable.Map[FaceType, Face]) extends Cube:
  override def isSolved: Boolean = state == Cube2x2.SOLVED_STATE

  override def printableState: String =
    f"     ${tileSymbol(FaceType.U,0)} ${tileSymbol(FaceType.U,1)}\n"+
    f"     ${tileSymbol(FaceType.U,2)} ${tileSymbol(FaceType.U,3)}\n"+
    f"${tileSymbol(FaceType.L,0)} ${tileSymbol(FaceType.L,1)}  "+
    f"${tileSymbol(FaceType.F,0)} ${tileSymbol(FaceType.F,1)}  "+
    f"${tileSymbol(FaceType.R,0)} ${tileSymbol(FaceType.R,1)}  "+
    f"${tileSymbol(FaceType.B,0)} ${tileSymbol(FaceType.B,1)}\n"+
    f"${tileSymbol(FaceType.L,2)} ${tileSymbol(FaceType.L,3)}  " +
    f"${tileSymbol(FaceType.F,2)} ${tileSymbol(FaceType.F,3)}  " +
    f"${tileSymbol(FaceType.R,2)} ${tileSymbol(FaceType.R,3)}  " +
    f"${tileSymbol(FaceType.B,2)} ${tileSymbol(FaceType.B,3)}\n"+
    f"     ${tileSymbol(FaceType.D,0)} ${tileSymbol(FaceType.D, 1)}\n" +
    f"     ${tileSymbol(FaceType.D,2)} ${tileSymbol(FaceType.D, 3)}\n"


object Cube2x2:
  private val SOLVED_STATE: String = "FFFFLLLLBBBBRRRRUUUUDDDD"

  def solved: Cube = Cube2x2(SOLVED_STATE)

  def solvedWithMask(mask: (Face, Tile) => Boolean): Cube = solved.applyMask(mask)

  def s(state: String, face: FaceType, index: Int): String = state.substring(face.index + index, face.index + index + 1)
  def f(state: String, face: FaceType): String = state.substring(face.index, face.index + 2*2)

  def apply(state: String): Cube =
    val faces: mutable.Map[FaceType, Face] = mutable.Map(
      FaceType.F -> Face(2, Axis.X, Axis.Y, FaceType.F, state.substring(FaceType.F.index, FaceType.F.index + 2 * 2)),
      FaceType.L -> Face(2, Axis.Zr, Axis.Y, FaceType.L, state.substring(FaceType.L.index, FaceType.L.index + 2 * 2)),
      FaceType.B -> Face(2, Axis.Xr, Axis.Y, FaceType.B, state.substring(FaceType.B.index, FaceType.B.index + 2 * 2)),
      FaceType.R -> Face(2, Axis.Z, Axis.Y, FaceType.R, state.substring(FaceType.R.index, FaceType.R.index + 2 * 2)),
      FaceType.U -> Face(2, Axis.X, Axis.Zr, FaceType.U, state.substring(FaceType.U.index, FaceType.U.index + 2 * 2)),
      FaceType.D -> Face(2, Axis.X, Axis.Z, FaceType.D, state.substring(FaceType.D.index, FaceType.D.index + 2 * 2)))
    Cube2x2(faces)

  def maskedEquals(state1: String, state2: String, mask: String): Boolean =
    mask zip (state1 zip state2) forall { case(m, (s1, s2)) => m == '0' || s1 == s2 }



sealed abstract class Move2x2(val symbol: String, val sliceAxis: Axis, val sliceCoords: Int, val face: FaceType,
                              val direction: MoveDirection):
  def applyToCube(cube: Cube): Cube =
    val faceRotated = cube.faces(face).rotated(direction)
    val slice = cube.slice(sliceAxis, sliceCoords)
    cube.withSliceRotated(faceRotated, slice)

  def edgeToEdge(origState: String, state: String,
                 faceFrom: FaceType, indexFrom1: Int, indexFrom2: Int,
                 faceTo: FaceType, indexTo1: Int, indexTo2: Int): String =
    replaceOne(origState,
      replaceOne(origState, state, faceFrom, indexFrom1, faceTo, indexTo1),
      faceFrom, indexFrom2, faceTo, indexTo2)

  private def replaceOne(origState: String, state: String,
                         faceFrom: FaceType, indexFrom: Int, faceTo: FaceType, indexTo: Int): String =
    state.substring(0, faceTo.index + indexTo) + Cube2x2.s(origState, faceFrom, indexFrom) +
      state.substring(faceTo.index + indexTo + 1)


object Moves2x2:
  case object F extends Move2x2("F", Axis.Z, 0, FaceType.F, MoveDirection.Natural)
  case object F1 extends Move2x2("F'", Axis.Zr, 0, FaceType.F, MoveDirection.Reversed)
  case object L extends Move2x2("L", Axis.Xr, 0, FaceType.L, MoveDirection.Reversed)
  case object L1 extends Move2x2("L'", Axis.X, 0, FaceType.L, MoveDirection.Natural)
  case object B extends Move2x2("B", Axis.Zr, 1, FaceType.B, MoveDirection.Reversed)
  case object B1 extends Move2x2("B'", Axis.Z, 1, FaceType.B, MoveDirection.Natural)
  case object R extends Move2x2("R", Axis.X, 1, FaceType.R, MoveDirection.Natural)
  case object R1 extends Move2x2("R'", Axis.Xr, 1, FaceType.R, MoveDirection.Reversed)
  case object U extends Move2x2("U", Axis.Y, 0, FaceType.U, MoveDirection.Reversed)
  case object U1 extends Move2x2("U'", Axis.Yr, 0, FaceType.U, MoveDirection.Natural)
  case object D extends Move2x2("D", Axis.Yr, 1, FaceType.D, MoveDirection.Natural)
  case object D1 extends Move2x2("D'", Axis.Y, 1, FaceType.D, MoveDirection.Reversed)

  private val all: Vector[Move2x2] = Vector(F, F1, L, L1, B, B1, R, R1, U, U1, D, D1)
  def random: Move2x2 = all(scala.util.Random.nextInt(all.length))
  def randomList(n: Int): Vector[Move2x2] =
    (0 until n).foldLeft(Vector[Move2x2]())((l, _) => l.appended(randomExceptOpposite(l.lastOption.map(_.symbol))))

  def randomExceptOpposite(prevSymbol: Option[String]): Move2x2 =
    val available = all.filterNot(s => prevSymbol.isDefined &&
      (s.symbol.substring(0,1) == prevSymbol.get.substring(0,1) && s.symbol.length != prevSymbol.get.length))
    available(scala.util.Random.nextInt(available.length))

  def from(symbol: String): Move2x2 = all.find(_.symbol == symbol).get


sealed abstract class FaceType(val symbol: String, val index: Int)

object FaceType:
  case object F extends FaceType("F", 0)
  case object L extends FaceType("L", 4)
  case object B extends FaceType("B", 8)
  case object R extends FaceType("R", 12)
  case object U extends FaceType("U", 16)
  case object D extends FaceType("D", 20)

  def apply(symbol: String): FaceType = symbol match
      case "F" => F
      case "L" => L
      case "B" => B
      case "R" => R
      case "U" => U
      case "D" => D

  private val faceOrderX: Vector[(FaceType, CoordsSort)] = Vector(
    (FaceType.F, CoordsSort.Ascending), (FaceType.U, CoordsSort.Descending),
    (FaceType.B, CoordsSort.Descending), (FaceType.D, CoordsSort.Ascending))
  private val faceOrderY: Vector[(FaceType, CoordsSort)] = Vector(
    (FaceType.F, CoordsSort.Ascending), (FaceType.L, CoordsSort.Descending),
    (FaceType.B, CoordsSort.Descending), (FaceType.R, CoordsSort.Ascending))
  private val faceOrderZ: Vector[(FaceType, CoordsSort)] = Vector(
    (FaceType.L, CoordsSort.Ascending), (FaceType.U, CoordsSort.Descending),
    (FaceType.R, CoordsSort.Descending), (FaceType.D, CoordsSort.Ascending))

  val faceOrder: Map[Axis, Vector[(FaceType, CoordsSort)]] =
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

case class TileCoords(c: Int, r: Int)

case class Tile(face: FaceType, coords: TileCoords, masked: Boolean = false)

sealed abstract class MoveDirection(val symbol: String)

object MoveDirection:
  case object Natural extends MoveDirection("N") // Clockwise when axis are not reversed
  case object Reversed extends MoveDirection("R") // Counterclockwise when axis are not reversed

sealed abstract class CoordsSort(val symbol: String)

object CoordsSort:
  case object Ascending extends CoordsSort("A")
  case object Descending extends CoordsSort("D")

case class Face(size: Int, axisH: Axis, axisV: Axis, nominalFace: FaceType, tiles: mutable.ArrayBuffer[Tile]):
  def state: String =
    // Using primitives for efficiency
    val len = tiles.length
    val chars = new Array[Char](len)
    var i = 0
    while i < len do
      chars(i) = tiles(i).face.symbol.charAt(0)
      i += 1
    new String(chars)

  def maskedState: String =
    // Using primitives for efficiency
    val len = tiles.length
    val chars = new Array[Char](len)
    var i = 0
    while i < len do
      val t = tiles(i)
      chars(i) = if t.masked then '.' else t.face.symbol.charAt(0)
      i += 1
    new String(chars)

  private def col(c: Int, sort: CoordsSort): mutable.ArrayBuffer[Tile] =
    val colTiles = tiles.filter(_.coords.c == c)
    sortTiles(colTiles, sort, axisV)

  private def row(r: Int, sort: CoordsSort): mutable.ArrayBuffer[Tile] =
    val rowTiles = tiles.filter(_.coords.r == r)
    sortTiles(rowTiles, sort, axisH)

  // Assume, that tiles are sorted in natural order
  private def sortTiles(sortTiles: mutable.ArrayBuffer[Tile], sort: CoordsSort, axis: Axis): mutable.ArrayBuffer[Tile] =
    if ((!axis.reversed && sort == CoordsSort.Descending) || (axis.reversed && sort == CoordsSort.Ascending))
      sortTiles.reverse
    else sortTiles

  def rotated(direction: MoveDirection): Face = direction match
    case MoveDirection.Natural => rotatedC
    case MoveDirection.Reversed => rotatedCC

  private def rotatedC: Face =
    // flip coordinates clockwise
    val newTiles = tiles.map(t =>
      val target = tiles.find(_.coords == TileCoords(t.coords.r, size - 1 - t.coords.c)).get
      t.copy(face = target.face, masked = target.masked)
    )
    tiles.clear()
    tiles.appendAll(newTiles)
    this

  private def rotatedCC: Face =
    // flip coordinates counterclockwise
    val newTiles = tiles.map(t =>
      val target = tiles.find(_.coords == TileCoords(size -1 - t.coords.r, t.coords.c)).get
      t.copy(face = target.face, masked = target.masked)
    )
    tiles.clear()
    tiles.appendAll(newTiles)
    this

  def edge(axis: Axis, i: Int, sort: CoordsSort): Edge =
    axis.symbol match
      case axisH.symbol => Edge(nominalFace, col(i, sort).toVector)
      case axisV.symbol => Edge(nominalFace, row(i, sort).toVector)

  def withEdge(currentEdge: Edge, newEdge: Edge): Face =
    val newTiles = currentEdge.tiles.indices.foldLeft(tiles)((t, i) =>
      val currentTile = currentEdge.tiles(i)
      val newTile = newEdge.tiles(i)
      val tileIndex = t.indexWhere(_.coords == currentTile.coords)
      t.updated(tileIndex, currentTile.copy(face = newTile.face, masked = newTile.masked)))
    tiles.clear()
    tiles.appendAll(newTiles)
    this

object Face:
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: FaceType): Face =
    val tiles = 0.until(size).flatMap(r => 0.until(size).map(c => Tile(nominalFace, TileCoords(r, c))))
    new Face(size, axisH, axisV, nominalFace, tiles.to(mutable.ArrayBuffer))

  // Tiles are always placed on a face in order: ABCD:
  // AB
  // CD
  // But they may have reversed coords. This simplifies textual state
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: FaceType, state: String): Face =
    val tiles = 0.until(state.length).map(i =>
      val r = if(axisH.reversed) size - (i % size) -1 else i % size
      val c = if(axisV.reversed) size - (i / size) -1 else i / size
      Tile(FaceType(state.substring(i, i + 1)), TileCoords(r, c))).toArray
    new Face(size, axisH, axisV, nominalFace, tiles.to(mutable.ArrayBuffer))

case class Edge(nominalFace: FaceType, tiles: Vector[Tile])

case class Slice(edges: Vector[Edge]):
  val edgePairs: Vector[(Edge, Edge)] = edges.zip(edges.drop(1).appended(edges.head))