package org.kr.cube

case class Environment(cube: Cube2x2, scrambleMoves: Int, history: Vector[EnvironmentLogEntry], expectedState: String):

  val state: String = cube.maskedState

  def step(action: String): Environment =
    val nextMove = Moves2x2.from(action)
    val stateBefore = state
    val cubeAfter = nextMove.applyToCube(cube)
    val stateAfter = cubeAfter.maskedState
    copy(cube = nextMove.applyToCube(cube), history = history.appended(EnvironmentLogEntry(stateBefore, action, stateAfter)))

  def isSolved: Boolean = state == expectedState

object Environment:
  def init(scrambleMoves: Int, expectedState: String, expectedMask: (Face, Tile) => Boolean): Environment =
    val scramble = Moves2x2.randomList(scrambleMoves)
    val initCube = Cube2x2.solvedWithMask(expectedMask)
    val randomCube = scramble.foldLeft(initCube)((c, m) => m.applyToCube(c))
    Environment(randomCube, scrambleMoves, Vector(), expectedState)


case class EnvironmentLogEntry(stateBefore: String, action: String, stateAfter: String)