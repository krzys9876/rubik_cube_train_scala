package org.kr.cube

import java.io.PrintWriter

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

case class Agent(qState: Map[String, (Int, Map[String, Double])]):
  val alpha: Double = 0.1
  val gamma: Double = 0.95
  val epsilon: Double = 0.2
  val epsilonDecay: Double = 0.999
  val epsilonMin: Double = 0.05
  val epsilonDecayEpisodes: Double = 200
  def updateEpisode(environment: Environment): Agent =
    if(!environment.isSolved || environment.history.isEmpty) this
    else doUpdateEpisode(environment)

  private def doUpdateEpisode(environment: Environment): Agent =
    val reward = 1.0
    val res = environment.history.reverse.foldLeft((qState, reward))({ case ((qs, g), h) =>
      val (oldActionCounter, oldQStates) = qs.getOrElse(h.stateBefore, (0, Map()))
      val oldActionWeight = oldQStates.getOrElse(h.action, 0.0)
      val newActionWeight = oldActionWeight + alpha * (g - oldActionWeight)
      val updatedQState = oldQStates.updated(h.action, newActionWeight)
      (qs.updated(h.stateBefore, (oldActionCounter + 1, updatedQState)), g * gamma)
    })
    copy(qState = res._1)

  def saveQState(filePath: String): Unit =
    val pw = new PrintWriter(filePath)
    pw.println(qState.map({case(k, v) => s"$k|${v._1}|${v._2.mkString("[","|","]")}"}).mkString("\n"))
    pw.close()