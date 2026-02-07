package org.kr.cube

import org.kr.cube.FaceType.{F, U}

import scala.collection.mutable

case class Cube3x3(override val faces: mutable.Map[FaceType, Face]) extends Cube:
  override def isSolved: Boolean = state == Cube3x3.SOLVED_STATE

  override def printableState: String =
    f"         ${tileSymbol(FaceType.U,0)}${tileSymbol(FaceType.U,1)}${tileSymbol(FaceType.U,2)}\n"+
    f"         ${tileSymbol(FaceType.U,3)}${tileSymbol(FaceType.U,4)}${tileSymbol(FaceType.U,5)}\n"+
    f"         ${tileSymbol(FaceType.U,6)}${tileSymbol(FaceType.U,7)}${tileSymbol(FaceType.U,8)}\n"+
      f"${tileSymbol(FaceType.L,0)}${tileSymbol(FaceType.L,1)}${tileSymbol(FaceType.L,2)}"+
      f"${tileSymbol(FaceType.F,0)}${tileSymbol(FaceType.F,1)}${tileSymbol(FaceType.F,2)}"+
      f"${tileSymbol(FaceType.R,0)}${tileSymbol(FaceType.R,1)}${tileSymbol(FaceType.R,2)}"+
      f"${tileSymbol(FaceType.B,0)}${tileSymbol(FaceType.B,1)}${tileSymbol(FaceType.B,2)}\n"+
      f"${tileSymbol(FaceType.L,3)}${tileSymbol(FaceType.L,4)}${tileSymbol(FaceType.L,5)}" +
      f"${tileSymbol(FaceType.F,3)}${tileSymbol(FaceType.F,4)}${tileSymbol(FaceType.F,5)}" +
      f"${tileSymbol(FaceType.R,3)}${tileSymbol(FaceType.R,4)}${tileSymbol(FaceType.R,5)}" +
      f"${tileSymbol(FaceType.B,3)}${tileSymbol(FaceType.B,4)}${tileSymbol(FaceType.B,5)}\n"+
      f"${tileSymbol(FaceType.L,6)}${tileSymbol(FaceType.L,7)}${tileSymbol(FaceType.L,8)}" +
      f"${tileSymbol(FaceType.F,6)}${tileSymbol(FaceType.F,7)}${tileSymbol(FaceType.F,8)}" +
      f"${tileSymbol(FaceType.R,6)}${tileSymbol(FaceType.R,7)}${tileSymbol(FaceType.R,8)}" +
      f"${tileSymbol(FaceType.B,6)}${tileSymbol(FaceType.B,7)}${tileSymbol(FaceType.B,8)}\n"+
      f"         ${tileSymbol(FaceType.D,0)}${tileSymbol(FaceType.D,1)}${tileSymbol(FaceType.D,2)}\n" +
      f"         ${tileSymbol(FaceType.D,3)}${tileSymbol(FaceType.D,4)}${tileSymbol(FaceType.D,5)}\n" +
      f"         ${tileSymbol(FaceType.D,6)}${tileSymbol(FaceType.D,7)}${tileSymbol(FaceType.D,8)}\n"

  private def t(face: FaceType, index: Int): (FaceType, Tile) = (face, faces(face).tiles(index))
  
  private def corners(): Vector[Vector[(FaceType, Tile)]] =
    Vector(
      Vector(t(FaceType.F, 0), t(FaceType.U, 6), t(FaceType.L, 2)),
      Vector(t(FaceType.L, 0), t(FaceType.U, 0), t(FaceType.B, 2)),
      Vector(t(FaceType.B, 0), t(FaceType.U, 2), t(FaceType.R, 2)),
      Vector(t(FaceType.R, 0), t(FaceType.U, 8), t(FaceType.F, 2)),
      Vector(t(FaceType.F, 6), t(FaceType.D, 0), t(FaceType.L, 8)),
      Vector(t(FaceType.L, 6), t(FaceType.D, 6), t(FaceType.B, 8)),
      Vector(t(FaceType.B, 6), t(FaceType.D, 8), t(FaceType.R, 8)),
      Vector(t(FaceType.R, 6), t(FaceType.D, 2), t(FaceType.F, 8)))

  private def edges(): Vector[Vector[(FaceType, Tile)]] =
    Vector(
      Vector(t(FaceType.F, 1), t(FaceType.U, 7)),
      Vector(t(FaceType.L, 1), t(FaceType.U, 3)),
      Vector(t(FaceType.B, 1), t(FaceType.U, 1)),
      Vector(t(FaceType.R, 1), t(FaceType.U, 5)),
      Vector(t(FaceType.F, 3), t(FaceType.L, 5)),
      Vector(t(FaceType.L, 3), t(FaceType.B, 5)),
      Vector(t(FaceType.B, 3), t(FaceType.R, 5)),
      Vector(t(FaceType.R, 3), t(FaceType.F, 5)),
      Vector(t(FaceType.F, 7), t(FaceType.D, 1)),
      Vector(t(FaceType.L, 7), t(FaceType.D, 3)),
      Vector(t(FaceType.B, 7), t(FaceType.D, 7)),
      Vector(t(FaceType.R, 7), t(FaceType.D, 5)))

  def centers(): Vector[(FaceType, Tile)] =
    Vector(t(FaceType.F, 4), t(FaceType.L, 4), t(FaceType.B, 4), t(FaceType.R, 4), t(FaceType.U, 4), t(FaceType.D, 4))

  def lowerEdges(): Vector[Vector[(FaceType, Tile)]] =
    edges().filter(c =>
      val cf = c.map(t => t._2.face)
      FaceType.lowerEdges.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def upperEdges(): Vector[Vector[(FaceType, Tile)]] =
    edges().filter(c =>
      val cf = c.map(t => t._2.face)
      FaceType.upperEdges.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def middleEdges(): Vector[Vector[(FaceType, Tile)]] =
    edges().filter(c =>
      val cf = c.map(t => t._2.face)
      FaceType.middleEdges.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def middleEdgesFL(): Vector[Vector[(FaceType, Tile)]] =
    val middleEdges = Vector(FaceType.middleEdges(0))
    edges().filter(c =>
      val cf = c.map(t => t._2.face)
      middleEdges.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def middleEdgesLB(): Vector[Vector[(FaceType, Tile)]] =
    val middleEdges = Vector(FaceType.middleEdges(0), FaceType.middleEdges(1))
    edges().filter(c =>
      val cf = c.map(t => t._2.face)
      middleEdges.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def middleEdgesBR(): Vector[Vector[(FaceType, Tile)]] =
    val middleEdges = Vector(FaceType.middleEdges(0), FaceType.middleEdges(1), FaceType.middleEdges(2))
    edges().filter(c =>
      val cf = c.map(t => t._2.face)
      middleEdges.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def upperCorners(): Vector[Vector[(FaceType, Tile)]] =
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      FaceType.upperCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def lowerCornerFL(): Vector[Vector[(FaceType, Tile)]] =
    val lowerCorners = Vector(FaceType.lowerCorners(0))
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      lowerCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  // We order add lower corners one by one
  def lowerCornersFL(): Vector[Vector[(FaceType, Tile)]] =
    val lowerCorners = Vector(FaceType.lowerCorners(0))
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      lowerCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def lowerCornersLB(): Vector[Vector[(FaceType, Tile)]] =
    val lowerCorners = Vector(FaceType.lowerCorners(0), FaceType.lowerCorners(1))
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      lowerCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def lowerCornersBR(): Vector[Vector[(FaceType, Tile)]] =
    val lowerCorners = Vector(FaceType.lowerCorners(0), FaceType.lowerCorners(1), FaceType.lowerCorners(2))
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      lowerCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  def lowerCornersAll(): Vector[Vector[(FaceType, Tile)]] =
    val lowerCorners = FaceType.lowerCorners
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      lowerCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))


object Cube3x3:
  val SOLVED_STATE: String = "FFFFFFFFFLLLLLLLLLBBBBBBBBBRRRRRRRRRUUUUUUUUUDDDDDDDDD"

  def solved: Cube = Cube3x3(SOLVED_STATE)
  def solved3x3: Cube3x3 = Cube3x3(SOLVED_STATE)

  def apply(state: String): Cube3x3 =
    val s = state.replaceAll(" ", "")
    val faces: mutable.Map[FaceType, Face] = mutable.Map(
      FaceType.F -> Face(3, Axis.X, Axis.Y, FaceType.F, s.substring(faceStateIndex(FaceType.F), faceStateIndex(FaceType.F) + 3 * 3)),
      FaceType.L -> Face(3, Axis.Zr, Axis.Y, FaceType.L, s.substring(faceStateIndex(FaceType.L), faceStateIndex(FaceType.L) + 3 * 3)),
      FaceType.B -> Face(3, Axis.Xr, Axis.Y, FaceType.B, s.substring(faceStateIndex(FaceType.B), faceStateIndex(FaceType.B) + 3 * 3)),
      FaceType.R -> Face(3, Axis.Z, Axis.Y, FaceType.R, s.substring(faceStateIndex(FaceType.R), faceStateIndex(FaceType.R) + 3 * 3)),
      FaceType.U -> Face(3, Axis.X, Axis.Zr, FaceType.U, s.substring(faceStateIndex(FaceType.U), faceStateIndex(FaceType.U) + 3 * 3)),
      FaceType.D -> Face(3, Axis.X, Axis.Z, FaceType.D, s.substring(faceStateIndex(FaceType.D), faceStateIndex(FaceType.D) + 3 * 3)))
    Cube3x3(faces)

  val faceStateIndex: Map[FaceType, Int] = Map(FaceType.F -> 0, FaceType.L -> 9, FaceType.B -> 18, FaceType.R -> 27,
    FaceType.U -> 36, FaceType.D -> 45)

  def maskAllExceptWhiteCross(cube: Cube3x3): Cube =
    val whiteCrossTiles = cube.lowerEdges().flatten ++ cube.centers()
    cube.maskTilesInverted(whiteCrossTiles)


  def maskUpperLayersFL(cube: Cube3x3): Cube =
    val whiteLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersFL().flatten
    cube.maskTilesInverted(whiteLayerTiles)

  def maskUpperLayersLB(cube: Cube3x3): Cube =
    val whiteLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersLB().flatten
    cube.maskTilesInverted(whiteLayerTiles)

  def maskUpperLayersBR(cube: Cube3x3): Cube =
    val whiteLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersBR().flatten
    cube.maskTilesInverted(whiteLayerTiles)

  def maskUpperLayersAll(cube: Cube3x3): Cube =
    val whiteLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersAll().flatten
    cube.maskTilesInverted(whiteLayerTiles)

  def maskUpperLayerFL(cube: Cube3x3): Cube =
    val upperLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersAll().flatten ++ cube.middleEdgesFL().flatten
    cube.maskTilesInverted(upperLayerTiles)

  def maskUpperLayerLB(cube: Cube3x3): Cube =
    val upperLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersAll().flatten ++ cube.middleEdgesLB().flatten
    cube.maskTilesInverted(upperLayerTiles)

  def maskUpperLayerBR(cube: Cube3x3): Cube =
    val upperLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersAll().flatten ++ cube.middleEdgesBR().flatten
    cube.maskTilesInverted(upperLayerTiles)

  def maskUpperLayerAll(cube: Cube3x3): Cube =
    val upperLayerTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersAll().flatten ++ cube.middleEdges().flatten
    cube.maskTilesInverted(upperLayerTiles)

  /*def maskYellowCross(cube: Cube3x3): Cube =
    val yellowCrossTiles = cube.lowerEdges().flatten ++ cube.centers() ++ cube.lowerCornersAll().flatten ++
      cube.middleEdges().flatten ++ cube.upperEdges().flatten
    cube.maskTilesInverted(yellowCrossTiles)*/


  def maskYellowCross(cube: Cube3x3): Cube =
    val upperLayerMasked = maskUpperLayerAll(cube)
    val yellowCrossTiles = cube.upperEdges().flatten.filter(ft => ft._2.face == FaceType.U)
    //val yellowCrossTiles12 = yellowCrossTiles.slice(0,2)
    cube.faces.values.foreach(f => cube.withFace(f.copy(tiles = f.tiles.map(t =>
      t.copy(masked = t.masked && !yellowCrossTiles.contains((f.nominalFace, t)))))))
    cube

  def maskYellowLayer(cube: Cube3x3): Cube =
    val upperLayerMasked = maskUpperLayerAll(cube)
    val yellowTiles = (cube.upperEdges() ++ cube.upperCorners()).flatten.filter(ft => ft._2.face == FaceType.U)
    cube.faces.values.foreach(f => cube.withFace(f.copy(tiles = f.tiles.map(t =>
      t.copy(masked = t.masked && !yellowTiles.contains((f.nominalFace, t)))))))
    cube

  def maskUpperEdges(cube: Cube3x3): Cube =
    val upperLayerMasked = maskYellowCross(cube)
    val upperEdgeTiles = cube.upperEdges().flatten.filter(ft => ft._2.face != FaceType.U)
    cube.faces.values.foreach(f => cube.withFace(f.copy(tiles = f.tiles.map(t =>
      t.copy(masked = upperEdgeTiles.contains((f.nominalFace, t)))))))
    cube


object Moves3x3:
  case object F extends Move("F", Axis.Z, 0, FaceType.F, MoveDirection.Natural)
  case object F1 extends Move("F'", Axis.Zr, 0, FaceType.F, MoveDirection.Reversed)
  case object L extends Move("L", Axis.Xr, 0, FaceType.L, MoveDirection.Reversed)
  case object L1 extends Move("L'", Axis.X, 0, FaceType.L, MoveDirection.Natural)
  case object B extends Move("B", Axis.Zr, 2, FaceType.B, MoveDirection.Reversed)
  case object B1 extends Move("B'", Axis.Z, 2, FaceType.B, MoveDirection.Natural)
  case object R extends Move("R", Axis.X, 2, FaceType.R, MoveDirection.Natural)
  case object R1 extends Move("R'", Axis.Xr, 2, FaceType.R, MoveDirection.Reversed)
  case object U extends Move("U", Axis.Y, 0, FaceType.U, MoveDirection.Reversed)
  case object U1 extends Move("U'", Axis.Yr, 0, FaceType.U, MoveDirection.Natural)
  case object D extends Move("D", Axis.Yr, 2, FaceType.D, MoveDirection.Natural)
  case object D1 extends Move("D'", Axis.Y, 2, FaceType.D, MoveDirection.Reversed)

  private val all: Vector[Move] = Vector(F, F1, L, L1, B, B1, R, R1, U, U1, D, D1)
  def random: Move = all(scala.util.Random.nextInt(all.length))
  def randomList(n: Int): Vector[Move] =
    (0 until n).foldLeft(Vector[Move]())((l, _) => l.appended(randomExceptOpposite(l.lastOption.map(_.symbol))))

  def randomExceptOpposite(prevSymbol: Option[String]): Move =
    val available = all.filterNot(s => prevSymbol.isDefined &&
      (s.symbol.substring(0,1) == prevSymbol.get.substring(0,1) && s.symbol.length != prevSymbol.get.length))
    available(scala.util.Random.nextInt(available.length))

  def from(symbol: String): Move = all.find(_.symbol == symbol).get

  val reverse: Map[Move, Move] = Map(
    F -> F1, L -> L1, B -> B1, R -> R1, U -> U1, D -> D1,
    F1 -> F, L1 -> L, B1 -> B, R1 -> R, U1 -> U, D1 -> D)
