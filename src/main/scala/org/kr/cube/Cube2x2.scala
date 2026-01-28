package org.kr.cube

case class Cube2x2(state: String):
  lazy val isSolved: Boolean = state == Cube2x2.SOLVED_STATE


object Cube2x2:
  private val SOLVED_STATE: String = "FFFFLLLLBBBBRRRRUUUUDDDD"

  def apply(state: String): Cube2x2 = new Cube2x2(state)
  def solved: Cube2x2 = Cube2x2(SOLVED_STATE)

