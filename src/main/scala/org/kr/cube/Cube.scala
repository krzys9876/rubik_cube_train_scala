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

  def withFace(face: Face): Cube =
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
      case "F" => "\u001B[44m\u001B[30m F \u001B[0m"
      case "L" => "\u001B[48;2;255;165;0m\u001B[30m L \u001B[0m"
      case "B" => "\u001B[42m\u001B[30m B \u001B[0m"
      case "R" => "\u001B[41m\u001B[30m R \u001B[0m"
      case "U" => "\u001B[43m\u001B[30m U \u001B[0m"
      case "D" => "\u001B[107m\u001B[30m D \u001B[0m"
    }

  def printableState: String

  def upperCorners(): Vector[Vector[(FaceType, Tile)]]
  def lowerCorners(): Vector[Vector[(FaceType, Tile)]]



sealed abstract class FaceType(val symbol: String)

object FaceType:
  case object F extends FaceType("F")
  case object L extends FaceType("L")
  case object B extends FaceType("B")
  case object R extends FaceType("R")
  case object U extends FaceType("U")
  case object D extends FaceType("D")

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

  val upperCorners: Vector[Vector[FaceType]] = Vector(
    Vector(FaceType.F, FaceType.U, FaceType.L),
    Vector(FaceType.L, FaceType.U, FaceType.B),
    Vector(FaceType.B, FaceType.U, FaceType.R),
    Vector(FaceType.R, FaceType.U, FaceType.F))

  val lowerCorners: Vector[Vector[FaceType]] = Vector(
    Vector(FaceType.F, FaceType.D, FaceType.L),
    Vector(FaceType.L, FaceType.D, FaceType.B),
    Vector(FaceType.B, FaceType.D, FaceType.R),
    Vector(FaceType.R, FaceType.D, FaceType.F))


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
    val tiles = 0.until(size).flatMap(c => 0.until(size).map(r => Tile(nominalFace, TileCoords(c, r))))
    new Face(size, axisH, axisV, nominalFace, tiles.to(mutable.ArrayBuffer))

  // Tiles are always placed on a face in order: ABCD:
  // AB
  // CD
  // But they may have reversed coords. This simplifies textual state
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: FaceType, state: String): Face =
    val tiles = 0.until(state.length).map(i =>
      val c = if(axisH.reversed) size - (i % size) -1 else i % size
      val r = if(axisV.reversed) size - (i / size) -1 else i / size
      Tile(FaceType(state.substring(i, i + 1)), TileCoords(c, r))).toArray
    new Face(size, axisH, axisV, nominalFace, tiles.to(mutable.ArrayBuffer))

case class Edge(nominalFace: FaceType, tiles: Vector[Tile])

case class Slice(edges: Vector[Edge]):
  val edgePairs: Vector[(Edge, Edge)] = edges.zip(edges.drop(1).appended(edges.head))


abstract class Move(val symbol: String, val sliceAxis: Axis, val sliceCoords: Int, val face: FaceType,
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
    state.substring(0, Cube2x2.faceStateIndex(faceTo) + indexTo) + Cube2x2.s(origState, faceFrom, indexFrom) +
      state.substring(Cube2x2.faceStateIndex(faceTo) + indexTo + 1)
