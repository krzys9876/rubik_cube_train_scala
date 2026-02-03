package org.kr.cube

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.mutable

@main
def main(): Unit =
  //trainRL2x2WhiteLayer()
  //testRun2x2WhiteLayer("q-values-2x2-white-layer-2000000-20260202_233105.txt")
  //trainRL2x2YellowLayer("solved-2x2-white-layer-1000000-20260203_005055.txt")
  //testRun2x2YellowLayer("q-values-2x2-white-layer-1000000-20260203_005055.txt", "q-values-2x2-yellow-layer-1000000-20260203_010048.txt")
  //trainRL2x2UpperLayer("solved-2x2-yellow-layer-1000000-20260203_010048.txt")
  testRun2x2UpperLayer(
    "q-values-2x2-white-layer-10000000-20260203_021603.txt",
    "q-values-2x2-yellow-layer-50000000-20260203_024944.txt",
    "q-values-2x2-upper-layer-10000000-20260203_094831.txt"
  )
  //debugUpperLayer("q-values-2x2-upper-layer-10000000-20260203_094831.txt")


/*def whiteLayer(): Unit =
  val max = 1000000
  val res = (0 until max).foldLeft(Vector[(String, String, Boolean, Vector[String])]())((l, i) =>
    val (initState, initLog, state, res, log) = trySolve(Cube2x2.solved, Cube2x2.solved.state, "001100110011001100001111", 200)
    if (res)
      println(f"$i $initState $state ${log.length} ${log.mkString(" ")} / ${initLog.length} ${initLog.mkString(" ")}")
      l.appended((initState, state, res, log))
    else l)
  println(f"solved in ${res.length} / $max attempts (${res.length.toDouble / max * 100.0}%.2f%%)")

def trySolve(cube: Cube2x2, state: String, mask: String, maxMoves: Int): (String, Vector[String], String, Boolean, Vector[String]) =
  val cube = Cube2x2.solved
  val scramble = Moves2x2.randomList(20)
  val randomCube = scramble.foldLeft(cube)((c, m) => m.applyToCube(c))

  @tailrec
  def makeRandomMoveUntil(cube: Cube2x2, targetState: String, mask: String, counter: Int, maxMoves: Int = 1000,
                          log: Vector[String] = Vector()): (Cube2x2, Boolean, Vector[String]) =
    if (Cube2x2.maskedEquals(cube.state, state, mask)) (cube, true, log)
    else if (counter >= maxMoves) (cube, false, log)
    else
      val nextMove = Moves2x2.randomExceptOpposite(log.lastOption)
      makeRandomMoveUntil(nextMove.applyToCube(cube), state, mask, counter + 1, maxMoves, log.appended(nextMove.symbol))

  val res = makeRandomMoveUntil(randomCube, state, mask, 0, maxMoves)
  val scrambleSymbols = scramble.map(_.symbol)
  (randomCube.state, scrambleSymbols, res._1.state, res._2, res._3)

*/


def printAgentStats(agent: Agent, max: Long): Unit =
  println(LocalDateTime.now())
  val agg = agent.qState.groupBy({ case (_, (c, _)) => c }).map((c, entries) => c -> entries.size).toVector.sortBy(_._1).reverse
  val singleVisited = agg.filter(_._1 == 1).map(_._2).sum
  val allOther = agg.filter(_._1 > 1).map(_._2).sum
  //println(f"q-values stats (number of visits - number of states): \n${agg.mkString("\n")}")
  println(f"q-values: ${agent.qState.keys.size} keys, single visits: $singleVisited, other: $allOther, ratio ${singleVisited.toDouble / (singleVisited+allOther).toDouble * 100.0}%.3f%%")
  println(f"episodes: ${agent.episodeCount}, episodes ratio of $max: ${agent.episodeCount.toDouble / max * 100.0}%.3f%%, epsilon: ${agent.epsilon}%.4f")

// NOTE: having low maximum number of moves (say 25) and high maximum iterations increases model accuracy
def episode(agent: Agent, env: Environment, maxMoves: Int): Environment =
  (0 until maxMoves).foldLeft(env)((e, i) =>
    val action = agent.nextBestTrainingAction(e)
    if (!e.isSolved) e.step(action) else e
  )

@tailrec
def iteration(agent: Agent, envGenerator: () => Environment, toGo: Long, initialMax: Long, maxMoves: Int, epochEpisodes: Long, successThisEpoch: Long): Agent =
  if (toGo == 0) agent
  else
    val initEnv = envGenerator()
    val afterEnvironment = episode(agent, initEnv, maxMoves)
    var nextSuccessCounter = if(afterEnvironment.isSolved) successThisEpoch + 1 else successThisEpoch
    if(toGo % epochEpisodes == 0)
      printAgentStats(agent, initialMax - toGo)
      println(f"epoch: ${EpochLog(epochEpisodes, nextSuccessCounter).toString}")
      nextSuccessCounter = 0
    iteration(agent.updateEpisode(afterEnvironment), envGenerator, toGo - 1, initialMax, maxMoves, epochEpisodes, nextSuccessCounter)

def trainRL2x2WhiteLayer(): Unit =
  val start = LocalDateTime.now()
  println(start)
  val max = 1000000
  val epochEpisodes = 50000
  val episodeMoves = 30
  val agent = Agent()
  val afterAgent = iteration(agent, () => Environment.init2x2WhiteLayerTraining(50), max, max, episodeMoves, epochEpisodes, 0)
  printAgentStats(afterAgent, max)
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  println("Saving q-values to file")
  afterAgent.saveQState(f"q-values-2x2-white-layer-$max-$timestampTxt.txt")
  afterAgent.saveSolvedStates(f"solved-2x2-white-layer-$max-$timestampTxt.txt")
  val end = LocalDateTime.now()
  println(end)
  val diffSec = start.until(end, ChronoUnit.SECONDS)
  println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

def testRun2x2WhiteLayer(filePath: String): Unit =
  println(f"Test run with: $filePath")
  val agent = Agent.load(filePath)
  println(f"Loaded: ${agent.qState.keys.size} records")
  val res = mutable.Map[Int, Int]()
  (0 until 100000).foreach(e =>
    val env = Environment.init2x2WhiteLayerTraining(20)
    (0 until 500).foreach(i =>
      if(!env.isSolved)
        env.step(agent.nextBestAction(env)))
    if(env.isSolved) res.update(env.history.length, res.getOrElse(env.history.length, 0) + 1)
    else res.update(-1, res.getOrElse(-1, 0) + 1)
  )
  println(res.toVector.sortBy(_._1).mkString("\n"))

def trainRL2x2YellowLayer(whileLayerSolvedFilePath: String): Unit =
  val start = LocalDateTime.now()
  println(start)
  val max = 1000000
  val epochEpisodes = 50000
  val episodeMoves = 100
  val solvedStates = loadSolved(whileLayerSolvedFilePath) // load solved states for white layer to begin with

  def prepareCube(): Cube2x2 =
    val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
    Cube2x2(solvedState).applyMask(Environment.yellowLayer2x2Selector)

  val agent = Agent()
  val afterAgent = iteration(agent, () => Environment.init(() => prepareCube(), Environment.yellowLayer2x2ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
  printAgentStats(afterAgent, max)
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  println("Saving q-values to file")
  afterAgent.saveQState(f"q-values-2x2-yellow-layer-$max-$timestampTxt.txt")
  afterAgent.saveSolvedStates(f"solved-2x2-yellow-layer-$max-$timestampTxt.txt")
  val end = LocalDateTime.now()
  println(end)
  val diffSec = start.until(end, ChronoUnit.SECONDS)
  println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


def loadSolved(filePath: String): Vector[String] =
  val source = scala.io.Source.fromFile(filePath)
  try
    source.getLines().toVector
  finally source.close()

def testRun2x2YellowLayer(filePathWhiteLayer: String, filePathYellowLayer: String): Unit =
  println(f"Test run with: $filePathWhiteLayer and $filePathYellowLayer")
  val whiteLayerAgent = Agent.load(filePathWhiteLayer)
  println(f"Loaded: ${whiteLayerAgent.qState.keys.size} records (white layer)")
  val yellowLayerAgent = Agent.load(filePathYellowLayer)
  println(f"Loaded: ${yellowLayerAgent.qState.keys.size} records (yellow layer)")
  val res = mutable.Map[Int, Int]()
  (0 until 100000).foreach(e =>
    if(e % 10000 == 0) println(e)
    val envWhiteLayer = Environment.init2x2WhiteLayerTraining(20)
    (0 until 500).foreach(i =>
      if(!envWhiteLayer.isSolved)
        envWhiteLayer.step(whiteLayerAgent.nextBestAction(envWhiteLayer)))
    if(envWhiteLayer.isSolved)
      val envYellowLayer = Environment.init(() => envWhiteLayer.cube.applyMask(Environment.yellowLayer2x2Selector), Environment.yellowLayer2x2ExpectedState)
      (0 until 500).foreach(i =>
        if (!envYellowLayer.isSolved)
          envYellowLayer.step(yellowLayerAgent.nextBestAction(envYellowLayer)))
      if(envYellowLayer.isSolved)
        val length = envWhiteLayer.history.length + envYellowLayer.history.length
        res.update(length, res.getOrElse(length, 0) + 1)
      else res.update(-1, res.getOrElse(-1, 0) + 1)
    else res.update(-1, res.getOrElse(-1, 0) + 1)
  )
  println(res.toVector.sortBy(_._1).mkString("\n"))

def trainRL2x2UpperLayer(yellowLayerSolvedFilePath: String): Unit =
  val start = LocalDateTime.now()
  println(start)
  val max = 10000000
  val epochEpisodes = 50000
  val episodeMoves = 30
  val solvedStates = loadSolved(yellowLayerSolvedFilePath) // load solved states for white layer to begin with

  def prepareCube(): Cube2x2 =
    val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
    Cube2x2(solvedState).applyMask(Environment.final2x2Selector)

  val agent = Agent(0.2, 0.001, 5000L) // minimum epsilon - we are looking for pattern, not for different ways to solve the upper layer
  val afterAgent = iteration(agent, () => Environment.init(() => prepareCube(), Environment.final2x2ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
  printAgentStats(afterAgent, max)
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  println("Saving q-values to file")
  afterAgent.saveQState(f"q-values-2x2-upper-layer-$max-$timestampTxt.txt")
  afterAgent.saveSolvedStates(f"solved-2x2-upper-layer-$max-$timestampTxt.txt")
  val end = LocalDateTime.now()
  println(end)
  val diffSec = start.until(end, ChronoUnit.SECONDS)
  println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

def testRun2x2UpperLayer(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String): Unit =
  println(f"Test run with: $filePathWhiteLayer, $filePathYellowLayer, $filePathUpperLayer")
  val whiteLayerAgent = Agent.load(filePathWhiteLayer)
  println(f"Loaded: ${whiteLayerAgent.qState.keys.size} records (white layer)")
  val yellowLayerAgent = Agent.load(filePathYellowLayer)
  println(f"Loaded: ${yellowLayerAgent.qState.keys.size} records (yellow layer)")
  val upperLayerAgent = Agent.load(filePathUpperLayer)
  println(f"Loaded: ${upperLayerAgent.qState.keys.size} records (upper layer)")

  //val solvedYellowStates = loadSolved("solved-2x2-yellow-layer-1000000-20260203_010048.txt")

  val res = mutable.Map[Int, Int]()
  val resUnsolved = mutable.Map[String, Int]()
  val resSolved = mutable.Map[String, Int]()
  (0 until 100000).foreach(e =>
    if(e % 10000 == 0) println(e)
    val initWhiteLayerEnv = Environment.init2x2WhiteLayerTraining(20)
    val envWhiteLayer = testRunStage(whiteLayerAgent, initWhiteLayerEnv, 500)
    if(envWhiteLayer.isSolved)
      val initYellowLayerEnv = Environment.init(() => envWhiteLayer.cube.applyMask(Environment.yellowLayer2x2Selector), Environment.yellowLayer2x2ExpectedState)
      val envYellowLayer = testRunStage(yellowLayerAgent, initYellowLayerEnv, 500)
      if(envYellowLayer.isSolved)
        val initUpperState = envYellowLayer.cube.state //if(solvedYellowStates.contains(envYellowLayer.cube.state)) println(envYellowLayer.cube.state)
        val initUpperLayerEnv = Environment.init(() => envYellowLayer.cube.applyMask(Environment.final2x2Selector), Environment.final2x2ExpectedState)
        val envUpperLayer = testRunStage(upperLayerAgent, initUpperLayerEnv, 500)
        if (envUpperLayer.isSolved)
          val length = envWhiteLayer.history.length + envYellowLayer.history.length + envUpperLayer.history.length
          resSolved.update(initUpperState, resSolved.getOrElse(initUpperState, 0) + 1)
          res.update(length, res.getOrElse(length, 0) + 1)
        else
          resUnsolved.update(initUpperState, resUnsolved.getOrElse(initUpperState, 0) + 1)
          res.update(-3, res.getOrElse(-3, 0) + 1)
      else res.update(-2, res.getOrElse(-2, 0) + 1)
    else res.update(-1, res.getOrElse(-1, 0) + 1)
  )
  println(res.toVector.sortBy(_._1).mkString("\n"))
  println("Unsolved")
  println(resUnsolved.toVector.sortBy(_._2).reverse.mkString("\n"))
  println("Solved")
  println(resSolved.toVector.sortBy(_._2).reverse.mkString("\n"))

def testRunStage(agent: Agent, environment: Environment, steps: Int): Environment =
  (0 until steps).foreach(_ => if (!environment.isSolved) environment.step(agent.nextBestAction(environment)))
  environment

def debugUpperLayer(filePathUpperLayer: String): Unit =
  val agent = Agent.load(filePathUpperLayer)
  println(f"Loaded: ${agent.qState.keys.size} records (upper layer)")
  val environment = Environment.init(() => Cube2x2("LRFFFBLLRLBBBFRRUUUUDDDD"), Environment.final2x2ExpectedState)
  val hist = mutable.ArrayBuffer[String]()
  (0 until 100).foreach(_ =>
    if (!environment.isSolved) {
      environment.step(agent.nextBestAction(environment))
      hist.append(f"${environment.history.last.toString}|${environment.history.last.action}|${environment.cube.state}|${environment.isSolved}")
    })
  if(environment.isSolved) println("Solved!")
  println(hist.mkString("\n"))

