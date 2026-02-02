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

case class Agent(qState: Map[String, (Int, Map[String, Double])], epsilon: Double = 0.2, episodeCount: Long = 0):
  private val alpha: Double = 0.1
  private val gamma: Double = 0.95
  private val epsilonDecay: Double = 0.95
  private val epsilonMin: Double = 0.05
  private val epsilonDecayEpisodes: Double = 100

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
    val newEpsilon =
      if(episodeCount > 0 && episodeCount % epsilonDecayEpisodes == 0) Math.max(epsilon * epsilonDecay, epsilonMin)
      else epsilon
    copy(qState = res._1, epsilon = newEpsilon, episodeCount = episodeCount + 1)

  def nextBestAction(environment: Environment): String =
    if(Math.random() < epsilon) nextRandomAction(environment)
    else
      val qValues = qState.getOrElse(environment.state, (0, Map()))._2
      if(qValues.isEmpty) nextRandomAction(environment)
      else
        val maxQ = qValues.values.max
        val bestList = qValues.filter(_._2 == maxQ)
        bestList.keys.toVector(scala.util.Random.nextInt(bestList.size))

  def nextRandomAction(environment: Environment): String = Moves2x2.randomExceptOpposite(None).symbol

  def saveQState(filePath: String): Unit =
    val pw = new PrintWriter(filePath)
    pw.println(qState.map({case(k, v) => s"$k|${v._1}|${v._2.mkString("[","|","]")}"}).mkString("\n"))
    pw.close()


object Agent:
  def apply(): Agent = Agent(Map())