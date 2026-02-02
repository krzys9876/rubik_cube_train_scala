package org.kr.cube

case class Cube2x2(faces: Map[Face2x2, Face]):

  private def f(face: Face2x2): String = faces(face).state
  private def s(face: Face2x2, index: Int): String = f(face).substring(index, index + 1)

  val state: String =
    faces(Face2x2.F).state + faces(Face2x2.L).state + faces(Face2x2.B).state + faces(Face2x2.R).state +
      faces(Face2x2.U).state + faces(Face2x2.D).state
  val maskedState: String =
    faces(Face2x2.F).maskedState + faces(Face2x2.L).maskedState + faces(Face2x2.B).maskedState + faces(Face2x2.R).maskedState +
      faces(Face2x2.U).maskedState + faces(Face2x2.D).maskedState
  val isSolved: Boolean = state == Cube2x2.SOLVED_STATE

  private def withFace(face: Face): Cube2x2 = Cube2x2(faces + (face.nominalFace -> face))

  def slice(axis: Axis, i: Int): Slice =
    val edges = Face2x2.faceOrder(axis).map({case(face, sort) => faces(face).edge(axis, i, sort)})
    Slice(edges)

  def withSliceRotated(faceRotated: Face, slice: Slice): Cube2x2 = 
    val newFaces = slice.edgePairs.map({case (eFrom, eTo) =>
      //NOTE: we must take original faces as we replace all faces, and we effectively overwrite the first with the last
      eTo.nominalFace -> this.faces(eTo.nominalFace).withEdge(eTo, eFrom)
    })
    copy(faces = faces ++ (newFaces :+ (faceRotated.nominalFace -> faceRotated)).toMap)
  


object Cube2x2:
  private val SOLVED_STATE: String = "FFFFLLLLBBBBRRRRUUUUDDDD"

  def solved: Cube2x2 = Cube2x2(SOLVED_STATE)

  def solvedWithMask(mask: (Face, Tile) => Boolean): Cube2x2 =
    val initCube = solved
    initCube.faces.values.foldLeft(initCube)((c, f) =>
      c.withFace(f.copy(tiles = f.tiles.map(t => t.copy(masked = mask(f, t))))))

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

  def maskedEquals(state1: String, state2: String, mask: String): Boolean =
    mask zip (state1 zip state2) forall { case(m, (s1, s2)) => m == '0' || s1 == s2 }



sealed abstract class Move2x2(val symbol: String, val sliceAxis: Axis, val sliceCoords: Int, val face: Face2x2,
                              val direction: MoveDirection):
  def applyToCube(cube: Cube2x2): Cube2x2 =
    val faceRotated = cube.faces(face).rotated(direction)
    val slice = cube.slice(sliceAxis, sliceCoords)
    cube.withSliceRotated(faceRotated, slice)

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
  case object F extends Move2x2("F", Axis.Z, 0, Face2x2.F, MoveDirection.Natural)
  case object F1 extends Move2x2("F'", Axis.Zr, 0, Face2x2.F, MoveDirection.Reversed)
  case object L extends Move2x2("L", Axis.Xr, 0, Face2x2.L, MoveDirection.Reversed)
  case object L1 extends Move2x2("L'", Axis.X, 0, Face2x2.L, MoveDirection.Natural)
  case object B extends Move2x2("B", Axis.Zr, 1, Face2x2.B, MoveDirection.Reversed)
  case object B1 extends Move2x2("B'", Axis.Z, 1, Face2x2.B, MoveDirection.Natural)
  case object R extends Move2x2("R", Axis.X, 1, Face2x2.R, MoveDirection.Natural)
  case object R1 extends Move2x2("R'", Axis.Xr, 1, Face2x2.R, MoveDirection.Reversed)
  case object U extends Move2x2("U", Axis.Y, 0, Face2x2.U, MoveDirection.Reversed)
  case object U1 extends Move2x2("U'", Axis.Yr, 0, Face2x2.U, MoveDirection.Natural)
  case object D extends Move2x2("D", Axis.Yr, 1, Face2x2.D, MoveDirection.Natural)
  case object D1 extends Move2x2("D'", Axis.Y, 1, Face2x2.D, MoveDirection.Reversed)

  private val all: Vector[Move2x2] = Vector(F, F1, L, L1, B, B1, R, R1, U, U1, D, D1)
  def random: Move2x2 = all(scala.util.Random.nextInt(all.length))
  def randomList(n: Int): Vector[Move2x2] =
    (0 until n).foldLeft(Vector[Move2x2]())((l, _) => l.appended(randomExceptOpposite(l.lastOption.map(_.symbol))))

  def randomExceptOpposite(prevSymbol: Option[String]): Move2x2 =
    val available = all.filterNot(s => prevSymbol.isDefined &&
      (s.symbol.substring(0,1) == prevSymbol.get.substring(0,1) && s.symbol.length != prevSymbol.get.length))
    available(scala.util.Random.nextInt(available.length))

  def from(symbol: String): Move2x2 = all.find(_.symbol == symbol).get


sealed abstract class Face2x2(val symbol: String, val index: Int)

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

  private val faceOrderX: Vector[(Face2x2, CoordsSort)] = Vector(
    (Face2x2.F, CoordsSort.Ascending), (Face2x2.U, CoordsSort.Descending),
    (Face2x2.B, CoordsSort.Descending), (Face2x2.D, CoordsSort.Ascending))
  private val faceOrderY: Vector[(Face2x2, CoordsSort)] = Vector(
    (Face2x2.F, CoordsSort.Ascending), (Face2x2.L, CoordsSort.Descending),
    (Face2x2.B, CoordsSort.Descending), (Face2x2.R, CoordsSort.Ascending))
  private val faceOrderZ: Vector[(Face2x2, CoordsSort)] = Vector(
    (Face2x2.L, CoordsSort.Ascending), (Face2x2.U, CoordsSort.Descending),
    (Face2x2.R, CoordsSort.Descending), (Face2x2.D, CoordsSort.Ascending))

  val faceOrder: Map[Axis, Vector[(Face2x2, CoordsSort)]] =
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

case class Tile(face: Face2x2, coords: TileCoords, masked: Boolean = false)

sealed abstract class MoveDirection(val symbol: String)

object MoveDirection:
  case object Natural extends MoveDirection("N") // Clockwise when axis are not reversed
  case object Reversed extends MoveDirection("R") // Counterclockwise when axis are not reversed

sealed abstract class CoordsSort(val symbol: String)

object CoordsSort:
  case object Ascending extends CoordsSort("A")
  case object Descending extends CoordsSort("D")

case class Face(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2, tiles: Vector[Tile]):
  private def col(c: Int, sort: CoordsSort): Vector[Tile] =
    val rowTiles = tiles.filter(_.coords.c == c)
    sort match
      case CoordsSort.Ascending => rowTiles.sortBy(_.coords.r)
      case CoordsSort.Descending => rowTiles.sortBy(_.coords.r).reverse

  private def row(r: Int, sort: CoordsSort): Vector[Tile] =
    val colTiles = tiles.filter(_.coords.r == r)
    sort match
      case CoordsSort.Ascending => colTiles.sortBy(_.coords.c)
      case CoordsSort.Descending => colTiles.sortBy(_.coords.c).reverse

  val state: String = tiles.map(_.face.symbol).mkString
  val maskedState: String = tiles.map(t => if(t.masked) "." else t.face.symbol).mkString

  def rotated(direction: MoveDirection): Face = direction match
    case MoveDirection.Natural => rotatedC
    case MoveDirection.Reversed => rotatedCC

  private def rotatedC: Face =
    // flip coordinates clockwise
    val newTiles = tiles.map(t =>
      val target = tiles.find(_.coords == TileCoords(t.coords.r, size - 1 - t.coords.c)).get
      t.copy(face = target.face, masked = target.masked)
    )
    copy(tiles = newTiles)

  private def rotatedCC: Face =
    // flip coordinates counterclockwise
    val newTiles = tiles.map(t =>
      val target = tiles.find(_.coords == TileCoords(size -1 - t.coords.r, t.coords.c)).get
      t.copy(face = target.face, masked = target.masked)
    )
    copy(tiles = newTiles)

  def edge(axis: Axis, i: Int, sort: CoordsSort): Edge =
    axis.symbol match
      case axisH.symbol => Edge(nominalFace, col(i, sort))
      case axisV.symbol => Edge(nominalFace, row(i, sort))

  def withEdge(currentEdge: Edge, newEdge: Edge): Face =
    val newTiles = currentEdge.tiles.indices.foldLeft(tiles)((t, i) =>
      val currentTile = currentEdge.tiles(i)
      val newTile = newEdge.tiles(i)
      val tileIndex = t.indexWhere(_.coords == currentTile.coords)
      t.updated(tileIndex, currentTile.copy(face = newTile.face, masked = newTile.masked)))
    copy(tiles = newTiles)

object Face:
  def apply(size: Int, axisH: Axis, axisV: Axis, nominalFace: Face2x2): Face =
    val tiles: Vector[Tile] = 0.until(size).flatMap(r => 0.until(size).map(c => Tile(nominalFace, TileCoords(r, c)))).toVector
    new Face(size, axisH, axisV, nominalFace, tiles)

  // Tiles are always placed on a face in order: ABCD:
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