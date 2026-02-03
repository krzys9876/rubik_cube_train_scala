package org.kr.cube

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


object Cube3x3:
  private val SOLVED_STATE: String = "FFFFFFFFFLLLLLLLLLBBBBBBBBBRRRRRRRRRUUUUUUUUUDDDDDDDDD"

  def solved: Cube = Cube3x3(SOLVED_STATE)

  def apply(state: String): Cube =
    val faces: mutable.Map[FaceType, Face] = mutable.Map(
      FaceType.F -> Face(3, Axis.X, Axis.Y, FaceType.F, state.substring(faceStateIndex(FaceType.F), faceStateIndex(FaceType.F) + 3 * 3)),
      FaceType.L -> Face(3, Axis.Zr, Axis.Y, FaceType.L, state.substring(faceStateIndex(FaceType.L), faceStateIndex(FaceType.L) + 3 * 3)),
      FaceType.B -> Face(3, Axis.Xr, Axis.Y, FaceType.B, state.substring(faceStateIndex(FaceType.B), faceStateIndex(FaceType.B) + 3 * 3)),
      FaceType.R -> Face(3, Axis.Z, Axis.Y, FaceType.R, state.substring(faceStateIndex(FaceType.R), faceStateIndex(FaceType.R) + 3 * 3)),
      FaceType.U -> Face(3, Axis.X, Axis.Zr, FaceType.U, state.substring(faceStateIndex(FaceType.U), faceStateIndex(FaceType.U) + 3 * 3)),
      FaceType.D -> Face(3, Axis.X, Axis.Z, FaceType.D, state.substring(faceStateIndex(FaceType.D), faceStateIndex(FaceType.D) + 3 * 3)))
    Cube3x3(faces)

  val faceStateIndex: Map[FaceType, Int] = Map(FaceType.F -> 0, FaceType.L -> 9, FaceType.B -> 18, FaceType.R -> 27,
    FaceType.U -> 36, FaceType.D -> 45)


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

