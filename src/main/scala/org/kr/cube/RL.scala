package org.kr.cube

case class Environment(cube: Cube2x2, scrambleMoves: Int, history: Vector[String],
                              expectedState: String, expectedMask: String):
  
  def state(): String = cube.state

  def step(action: String): Environment =
    val nextMove = Moves2x2.from(action)
    copy(cube = nextMove.applyToCube(cube), history = history.appended(action))

  def isSolved: Boolean = Cube2x2.maskedEquals(state(), expectedState, expectedMask)

object Environment:
  def init(scrambleMoves: Int, expectedState: String, expectedMask: String): Environment =
    val scramble = Moves2x2.randomList(scrambleMoves)
    val randomCube = scramble.foldLeft(Cube2x2.solved)((c, m) => m.applyToCube(c))
    Environment(randomCube, scrambleMoves, Vector(), expectedState, expectedMask)
