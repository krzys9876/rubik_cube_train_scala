package org.kr.cube

import scala.collection.mutable

case class Cube2x2(override val faces: mutable.Map[FaceType, Face]) extends Cube:
  override def isSolved: Boolean = state == Cube2x2.SOLVED_STATE

  override def printableState: String =
    f"      ${tileSymbol(FaceType.U,0)}${tileSymbol(FaceType.U,1)}\n"+
      f"      ${tileSymbol(FaceType.U,2)}${tileSymbol(FaceType.U,3)}\n"+
      f"${tileSymbol(FaceType.L,0)}${tileSymbol(FaceType.L,1)}"+
      f"${tileSymbol(FaceType.F,0)}${tileSymbol(FaceType.F,1)}"+
      f"${tileSymbol(FaceType.R,0)}${tileSymbol(FaceType.R,1)}"+
      f"${tileSymbol(FaceType.B,0)}${tileSymbol(FaceType.B,1)}\n"+
      f"${tileSymbol(FaceType.L,2)}${tileSymbol(FaceType.L,3)}" +
      f"${tileSymbol(FaceType.F,2)}${tileSymbol(FaceType.F,3)}" +
      f"${tileSymbol(FaceType.R,2)}${tileSymbol(FaceType.R,3)}" +
      f"${tileSymbol(FaceType.B,2)}${tileSymbol(FaceType.B,3)}\n"+
      f"      ${tileSymbol(FaceType.D,0)}${tileSymbol(FaceType.D, 1)}\n" +
      f"      ${tileSymbol(FaceType.D,2)}${tileSymbol(FaceType.D, 3)}\n"

  private def corners(): Vector[Vector[(FaceType, Tile)]] =
    def corner(face: FaceType, index: Int): (FaceType, Tile) = (face, faces(face).tiles(index))
    Vector(
      Vector(corner(FaceType.F, 0), corner(FaceType.U, 2), corner(FaceType.L, 1)),
      Vector(corner(FaceType.L, 0), corner(FaceType.U, 0), corner(FaceType.B, 1)),
      Vector(corner(FaceType.B, 0), corner(FaceType.U, 1), corner(FaceType.R, 1)),
      Vector(corner(FaceType.R, 0), corner(FaceType.U, 3), corner(FaceType.F, 1)),
      Vector(corner(FaceType.F, 2), corner(FaceType.D, 0), corner(FaceType.L, 3)),
      Vector(corner(FaceType.L, 2), corner(FaceType.D, 2), corner(FaceType.B, 3)),
      Vector(corner(FaceType.B, 2), corner(FaceType.D, 3), corner(FaceType.R, 3)),
      Vector(corner(FaceType.R, 2), corner(FaceType.D, 1), corner(FaceType.F, 3)))

  override def upperCorners(): Vector[Vector[(FaceType, Tile)]] =
    corners().filter(c =>
      val cf = c.map(t => t._2.face)
      FaceType.upperCorners.exists(uc => uc.sortBy(_.symbol).equals(cf.sortBy(_.symbol))))

  override def lowerCorners(): Vector[Vector[(FaceType, Tile)]] = ???


object Cube2x2:
  private val SOLVED_STATE: String = "FFFFLLLLBBBBRRRRUUUUDDDD"

  def solved: Cube = Cube2x2(SOLVED_STATE)

  def solvedWithMask(mask: (Face, Tile) => Boolean): Cube = solved.applyMask(mask)

  def s(state: String, face: FaceType, index: Int): String = state.substring(faceStateIndex(face) + index, faceStateIndex(face) + index + 1)

  def apply(state: String): Cube =
    val faces: mutable.Map[FaceType, Face] = mutable.Map(
      FaceType.F -> Face(2, Axis.X, Axis.Y, FaceType.F, state.substring(faceStateIndex(FaceType.F), faceStateIndex(FaceType.F) + 2 * 2)),
      FaceType.L -> Face(2, Axis.Zr, Axis.Y, FaceType.L, state.substring(faceStateIndex(FaceType.L), faceStateIndex(FaceType.L) + 2 * 2)),
      FaceType.B -> Face(2, Axis.Xr, Axis.Y, FaceType.B, state.substring(faceStateIndex(FaceType.B), faceStateIndex(FaceType.B) + 2 * 2)),
      FaceType.R -> Face(2, Axis.Z, Axis.Y, FaceType.R, state.substring(faceStateIndex(FaceType.R), faceStateIndex(FaceType.R) + 2 * 2)),
      FaceType.U -> Face(2, Axis.X, Axis.Zr, FaceType.U, state.substring(faceStateIndex(FaceType.U), faceStateIndex(FaceType.U) + 2 * 2)),
      FaceType.D -> Face(2, Axis.X, Axis.Z, FaceType.D, state.substring(faceStateIndex(FaceType.D), faceStateIndex(FaceType.D) + 2 * 2)))
    Cube2x2(faces)

  def maskedEquals(state1: String, state2: String, mask: String): Boolean =
    mask zip (state1 zip state2) forall { case(m, (s1, s2)) => m == '0' || s1 == s2 }

  def maskUpperCorners(cube: Cube): Cube =
    val upperTiles = cube.upperCorners().flatten
    cube.faces.values.foreach(f => cube.withFace(f.copy(tiles = f.tiles.map(t =>
      t.copy(masked = upperTiles.contains((f.nominalFace, t)))))))
    cube

  val faceStateIndex: Map[FaceType, Int] = Map(FaceType.F -> 0, FaceType.L -> 4, FaceType.B -> 8, FaceType.R -> 12,
    FaceType.U -> 16, FaceType.D -> 20)


object Moves2x2:
  case object F extends Move("F", Axis.Z, 0, FaceType.F, MoveDirection.Natural)
  case object F1 extends Move("F'", Axis.Zr, 0, FaceType.F, MoveDirection.Reversed)
  case object L extends Move("L", Axis.Xr, 0, FaceType.L, MoveDirection.Reversed)
  case object L1 extends Move("L'", Axis.X, 0, FaceType.L, MoveDirection.Natural)
  case object B extends Move("B", Axis.Zr, 1, FaceType.B, MoveDirection.Reversed)
  case object B1 extends Move("B'", Axis.Z, 1, FaceType.B, MoveDirection.Natural)
  case object R extends Move("R", Axis.X, 1, FaceType.R, MoveDirection.Natural)
  case object R1 extends Move("R'", Axis.Xr, 1, FaceType.R, MoveDirection.Reversed)
  case object U extends Move("U", Axis.Y, 0, FaceType.U, MoveDirection.Reversed)
  case object U1 extends Move("U'", Axis.Yr, 0, FaceType.U, MoveDirection.Natural)
  case object D extends Move("D", Axis.Yr, 1, FaceType.D, MoveDirection.Natural)
  case object D1 extends Move("D'", Axis.Y, 1, FaceType.D, MoveDirection.Reversed)

  private val all: Vector[Move] = Vector(F, F1, L, L1, B, B1, R, R1, U, U1, D, D1)
  def random: Move = all(scala.util.Random.nextInt(all.length))
  def randomList(n: Int): Vector[Move] =
    (0 until n).foldLeft(Vector[Move]())((l, _) => l.appended(randomExceptOpposite(l.lastOption.map(_.symbol))))

  def randomExceptOpposite(prevSymbol: Option[String]): Move =
    val available = all.filterNot(s => prevSymbol.isDefined &&
      (s.symbol.substring(0,1) == prevSymbol.get.substring(0,1) && s.symbol.length != prevSymbol.get.length))
    available(scala.util.Random.nextInt(available.length))

  def from(symbol: String): Move = all.find(_.symbol == symbol).get

