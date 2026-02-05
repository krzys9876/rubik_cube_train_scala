package org.kr.cube.rl

import org.kr.cube.*

import java.io.PrintWriter
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

case class Environment(var cube: Cube, history: mutable.ArrayBuffer[EnvironmentLogEntry],
                       expectedState: String, var state: String, initScramble: Vector[String],
                       moveDecoder: MoveDecoder):

  def step(action: String, printState: Boolean = false): Environment =
    val nextMove = moveDecoder.decodeMove(action)
    val stateBefore = state
    cube = nextMove.applyToCube(cube)
    state = cube.maskedState
    history.append(EnvironmentLogEntry(stateBefore, action))
    hasLoop = detectLoop(state)
    if(printState) println(f"\naction: $action -> \n\n${cube.printableState}")
    this

  @tailrec
  final def trainingStep(agent: Agent, attempts: Int = 10): Environment =
    if(attempts == 0) this
    else
      val action = agent.nextBestTrainingAction(this, 0.001)
      val nextMove = moveDecoder.decodeMove(action)
      val stateBefore = state
      cube = nextMove.applyToCube(cube)
      state = cube.maskedState
      if(stateBefore != state || detectLoop(state))
        history.append(EnvironmentLogEntry(stateBefore, action))
        hasLoop = detectLoop(state)
        this
      else
        cube = moveDecoder.reverse(nextMove).applyToCube(cube)
        trainingStep(agent, attempts - 1)


  def isSolved: Boolean = state == expectedState
  def success: Boolean = isSolved && !hasLoop

  private var hasLoop: Boolean = false
  private def detectLoop(state: String): Boolean = history.exists(_.stateBefore == state)


object Environment:
  def init2x2(cubeGenerator: () => Cube, expectedState: String): Environment =
    val cube = cubeGenerator()
    Environment(cube, mutable.ArrayBuffer(), expectedState, cube.maskedState, Vector(), MoveDecoder2x2())

  def init3x3(cubeGenerator: () => Cube, expectedState: String): Environment =
    val cube = cubeGenerator()
    Environment(cube, mutable.ArrayBuffer(), expectedState, cube.maskedState, Vector(), MoveDecoder3x3())

  def init2x2WhiteLayerTraining(scrambleMoves: Int): Environment =
    val scramble = Moves2x2.randomList(scrambleMoves)
    val initCube = Cube2x2.maskUpperCorners(Cube2x2.solved2x2)
    val randomCube = scramble.foldLeft(initCube)((c, m) => m.applyToCube(c))
    Environment(randomCube, mutable.ArrayBuffer(), whiteLayer2x2ExpectedState, randomCube.maskedState, scramble.map(_.symbol), MoveDecoder2x2())

  val whiteLayer2x2ExpectedState: String = "..FF..LL..BB..RR....DDDD"
  val yellowLayer2x2ExpectedState: String = "..FF..LL..BB..RRUUUUDDDD"
  val final2x2ExpectedState: String = Cube2x2.SOLVED_STATE

  val whiteCross3x3ExpectedState: String = "....F..F.....L..L.....B..B.....R..R.....U.....D.DDD.D."
  //val whiteLayer3x3ExpectedState: String = "....F.FFF....L.LLL....B.BBB....R.RRR....U....DDDDDDDDD"
  //val whiteLayer3x3ExpectedState: String = "....F.FF.....L.LLL....B..BB....R..R.....U....DD.DDDDD."
  val whiteLayer3x3ExpectedState: String = "....F.FF.....L..LL....B..B.....R..R.....U....DD.DDD.D."

  def init3x3WhiteCrossTraining(scrambleMoves: Int): Environment =
    val scramble = Moves3x3.randomList(scrambleMoves)
    val initCube = Cube3x3.maskAllExceptWhiteCross(Cube3x3.solved3x3)
    val randomCube = scramble.foldLeft(initCube)((c, m) => m.applyToCube(c))
    Environment(randomCube, mutable.ArrayBuffer(), whiteCross3x3ExpectedState, randomCube.maskedState, scramble.map(_.symbol), MoveDecoder3x3())

  def init3x3WhiteLayerTraining(scrambleMoves: Int): Environment =
    val scramble = Moves3x3.randomList(scrambleMoves)
    val initCube = Cube3x3.maskUpperLayers(Cube3x3.solved3x3)
    val randomCube = scramble.foldLeft(initCube)((c, m) => m.applyToCube(c))
    Environment(randomCube, mutable.ArrayBuffer(), whiteLayer3x3ExpectedState, randomCube.maskedState, scramble.map(_.symbol), MoveDecoder3x3())


case class EnvironmentLogEntry(stateBefore: String, action: String)

case class Agent(qState: Map[String, (Int, Map[String, Double])], solvedStates: immutable.Set[String],
                 epsilon: Double = 0.25, epsilonMin: Double = 0.05, epsilonDecayEpisodes: Double = 1500, episodeCount: Long = 0):
  private val alpha: Double = 0.1
  private val gamma: Double = 0.95
  private val epsilonDecay: Double = 0.99

  def updateEpisode(environment: Environment): Agent =
    if(environment.history.isEmpty) this
    else doUpdateEpisode(environment)

  private def doUpdateEpisode(environment: Environment): Agent =
    val (reward, counterIncrease) = if(environment.success) (1.0, 1) else (0.0, 0)
    val res = environment.history.reverse.foldLeft((qState, reward))({ case ((qs, g), h) =>
      val (oldActionCounter, oldQStates) = qs.getOrElse(h.stateBefore, (0, Map()))
      val oldActionWeight = oldQStates.getOrElse(h.action, 0.0)
      val newActionWeight = oldActionWeight + alpha * (g - oldActionWeight)
      val updatedQState =
        // Do not store zeros as it bloats the q-states
        if(newActionWeight != 0.0) oldQStates.updated(h.action, newActionWeight)
        else
          if(oldQStates.get(h.action).contains(0.0))
            oldQStates.removed(h.action)
          else oldQStates
      val newQs =
        if(updatedQState.isEmpty) qs.removed(h.stateBefore)
        else qs.updated(h.stateBefore, (oldActionCounter + 1, updatedQState))
      (newQs, g * gamma)
    })
    val newEpsilon = {
      // Do not store empty keys
      if(episodeCount > 0 && episodeCount % epsilonDecayEpisodes == 0) Math.max(epsilon * epsilonDecay, epsilonMin)
      else epsilon
    }
    val newSolvedStates = if(environment.isSolved) solvedStates + environment.cube.state else solvedStates
    copy(qState = res._1, epsilon = newEpsilon, episodeCount = episodeCount + counterIncrease, solvedStates = newSolvedStates)

  def nextBestTrainingAction(environment: Environment, scale: Double = 0.0): String =
    if(Math.random() < epsilon) nextRandomAction(environment)
    else nextBestAction(environment, scale)

  def nextBestAction(environment: Environment, scale: Double = 0.0, epsilon: Double = 0.0): String =
    if(Math.random() < epsilon) nextRandomAction(environment)
    else
      val qValues = qState.getOrElse(environment.state, (0, Map()))._2
      if (qValues.isEmpty) nextRandomAction(environment)
      else
        val maxQ = qValues.values.max
        val bestList = qValues.filter(v => (scale == 0.0 && v._2 == maxQ) || Math.abs(v._2 - maxQ) < scale)
        bestList.keys.toVector(scala.util.Random.nextInt(bestList.size))

  def nextRandomAction(environment: Environment): String = environment.moveDecoder.randomMove(environment.history.lastOption.map(_.action))

  def saveQState(filePath: String): Unit =
    val pw = new PrintWriter(filePath)
    pw.println(qState.map({case(k, v) => s"$k|${v._1}|${v._2.toVector.map(e => e._1 + ":" + e._2).mkString("#")}"}).mkString("\n"))
    pw.close()

  def saveSolvedStates(filePath: String): Unit =
    val pw = new PrintWriter(filePath)
    pw.println(solvedStates.toVector.mkString("\n"))
    pw.close()

  def printStats(max: Long): Unit =
    println(LocalDateTime.now())
    val agg = qState.groupBy({ case (_, (c, _)) => c }).map((c, entries) => c -> entries.size).toVector.sortBy(_._1).reverse
    val singleVisited = agg.filter(_._1 == 1).map(_._2).sum
    val allOther = agg.filter(_._1 > 1).map(_._2).sum
    //println(f"q-values stats (number of visits - number of states): \n${agg.mkString("\n")}")
    println(f"q-values: ${qState.keys.size} keys, single visits: $singleVisited, other: $allOther, ratio ${singleVisited.toDouble / (singleVisited + allOther).toDouble * 100.0}%.3f%%")
    println(f"episodes: ${episodeCount}, episodes ratio of $max: ${episodeCount.toDouble / max * 100.0}%.3f%%, epsilon: ${epsilon}%.4f")


object Agent:
  def apply(): Agent = Agent(Map(), immutable.Set())

  def apply(epsilonInit: Double, epsilonMin: Double, epsilonDecayEpisodes: Long): Agent =
    Agent(Map(), immutable.Set(), epsilonInit, epsilonMin, epsilonDecayEpisodes)

  def load(filePath: String, epsilonInit: Double = 0.2, epsilonMin: Double = 0.05, epsilonDecayEpisodes: Long = 1000L): Agent =
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

abstract class MoveDecoder:
  def decodeMove(symbol: String): Move
  def randomMove(except: Option[String]): String
  def reverse(move: Move): Move

case class MoveDecoder2x2() extends MoveDecoder:
  override def decodeMove(symbol: String): Move = Moves2x2.from(symbol)
  override def randomMove(except: Option[String]): String = Moves2x2.randomExceptOpposite(except).symbol
  override def reverse(move: Move): Move = Moves2x2.reverse(move)

case class MoveDecoder3x3() extends MoveDecoder:
  override def decodeMove(symbol: String): Move = Moves3x3.from(symbol)
  override def randomMove(except: Option[String]): String = Moves3x3.randomExceptOpposite(except).symbol
  override def reverse(move: Move): Move = Moves3x3.reverse(move)