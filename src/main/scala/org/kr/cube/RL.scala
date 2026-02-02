package org.kr.cube

import java.io.PrintWriter
import scala.collection.{immutable, mutable}

case class Environment(var cube: Cube2x2, history: mutable.ArrayBuffer[EnvironmentLogEntry],
                       expectedState: String, var state: String):

  def step(action: String): Environment =
    val nextMove = Moves2x2.from(action)
    val stateBefore = state
    cube = nextMove.applyToCube(cube)
    state = cube.maskedState
    history.append(EnvironmentLogEntry(stateBefore, action))
    this

  def isSolved: Boolean = state == expectedState

object Environment:
  def init(scrambleMoves: Int, expectedState: String, expectedMask: (Face, Tile) => Boolean): Environment =
    val scramble = Moves2x2.randomList(scrambleMoves)
    val initCube = Cube2x2.solvedWithMask(expectedMask)
    val randomCube = scramble.foldLeft(initCube)((c, m) => m.applyToCube(c))
    Environment(randomCube, mutable.ArrayBuffer(), expectedState, randomCube.maskedState)

  def init(cubeGenerator: () => Cube2x2, expectedState: String): Environment =
    val cube = cubeGenerator()
    Environment(cube, mutable.ArrayBuffer(), expectedState, cube.maskedState)

  def init2x2WhiteLayerTraining(scrambleMoves: Int): Environment =
    init(scrambleMoves, whiteLayer2x2ExpectedState, whiteLayer2x2Selector)

  private val whiteLayer2x2ExpectedState: String = "..FF..LL..BB..RR....DDDD"
  private val whiteLayer2x2Selector: (Face, Tile) => Boolean =
    (f: Face, t: Tile) => f.nominalFace == Face2x2.U || (f.axisV.symbol == "Y" && t.coords.r == 0)

  def init2x2YellowLayerTraining(scrambleMoves: Int): Environment =
    init(scrambleMoves, yellowLayer2x2ExpectedState, yellowLayer2x2Selector)

  val yellowLayer2x2ExpectedState: String = "..FF..LL..BB..RRUUUUDDDD"
  val yellowLayer2x2Selector: (Face, Tile) => Boolean =
    (f: Face, t: Tile) => t.face != Face2x2.U && !(f.axisV.symbol == "Y" && t.coords.r == 1 || f.nominalFace == Face2x2.D)


case class EnvironmentLogEntry(stateBefore: String, action: String)

case class Agent(qState: Map[String, (Int, Map[String, Double])], solvedStates: immutable.Set[String], epsilon: Double = 0.25, episodeCount: Long = 0):
  private val alpha: Double = 0.1
  private val gamma: Double = 0.95
  private val epsilonDecay: Double = 0.99
  private val epsilonMin: Double = 0.05
  private val epsilonDecayEpisodes: Double = 1500

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
    copy(qState = res._1, epsilon = newEpsilon, episodeCount = episodeCount + 1, solvedStates = solvedStates + environment.state)

  def nextBestTrainingAction(environment: Environment): String =
    if(Math.random() < epsilon) nextRandomAction(environment)
    else nextBestAction(environment)

  def nextBestAction(environment: Environment): String =
    if (Math.random() < epsilon) nextRandomAction(environment)
    else
      val qValues = qState.getOrElse(environment.state, (0, Map()))._2
      if (qValues.isEmpty) nextRandomAction(environment)
      else
        val maxQ = qValues.values.max
        val bestList = qValues.filter(_._2 == maxQ)
        bestList.keys.toVector(scala.util.Random.nextInt(bestList.size))

  def nextRandomAction(environment: Environment): String = Moves2x2.randomExceptOpposite(None).symbol

  def saveQState(filePath: String): Unit =
    val pw = new PrintWriter(filePath)
    pw.println(qState.map({case(k, v) => s"$k|${v._1}|${v._2.toVector.map(e => e._1 + ":" + e._2).mkString("#")}"}).mkString("\n"))
    pw.close()

  def saveSolvedStates(filePath: String): Unit =
    val pw = new PrintWriter(filePath)
    pw.println(solvedStates.toVector.mkString("\n"))
    pw.close()


object Agent:
  def apply(): Agent = Agent(Map(), immutable.Set())

  def load(filePath: String): Agent =
    val source = scala.io.Source.fromFile(filePath)
    try
      // each row contains: key (cube state), counter, map of action -> q-value (double)
      val qState = source.getLines().toVector
        .map(_.split('|')) // top level elements
        .map({case Array(k, c, v) =>
          (k, (c.toInt, v.split('#') // key, counter, actions (to be extracted from hash-separated string)
            .map(_.split(':')) // separate action and q-value
            .map({case Array(m, q) => (m, q.toDouble)}).toMap))}).toMap
      Agent(qState, immutable.Set())
    finally source.close()


case class EpochLog(episodeCount: Long, successCount: Long):
  override def toString: String = f"episodes: $episodeCount, success: $successCount, ratio: ${successCount.toDouble / episodeCount.toDouble * 100.0}%.3f%%"