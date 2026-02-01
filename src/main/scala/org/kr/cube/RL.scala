package org.kr.cube

case class Environment(cube: Cube2x2, scrambleMoves: Int, history: Vector[String], expectedState: String):

  val state: String = cube.maskedState

  def step(action: String): Environment =
    val nextMove = Moves2x2.from(action)
    copy(cube = nextMove.applyToCube(cube), history = history.appended(action))

  def isSolved: Boolean = state == expectedState

object Environment:
  def init(scrambleMoves: Int, expectedState: String, expectedMask: (Face, Tile) => Boolean): Environment =
    val scramble = Moves2x2.randomList(scrambleMoves)
    val initCube = Cube2x2.solvedWithMask(expectedMask)
    val randomCube = scramble.foldLeft(initCube)((c, m) => m.applyToCube(c))
    Environment(randomCube, scrambleMoves, Vector(), expectedState)
