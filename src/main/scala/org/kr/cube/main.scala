package org.kr.cube

import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.mutable

@main
def main(): Unit = {
  //trainRL2x2WhiteLayer()
  //testRun2x2WhiteLayer("q-values-2x2-white-layer-3000000-20260203_110743.txt")
  //testRun2x2WhiteLayer("re-q-values-2x2-white-layer-500000-20260203_133029.txt")
  //reTrainRL2x2WhiteLayer("q-values-2x2-white-layer-3000000-20260203_110743.txt", "unsolved-2x2-white-layer-20260203_124031.txt")
  //reTrainRL2x2WhiteLayer("re-q-values-2x2-white-layer-500000-20260203_131910.txt", "unsolved-2x2-white-layer-20260203_131925.txt")
  //reTrainAndTestRL2x2WhiteLayer("re-q-values-2x2-white-layer-500000-20260203_130753.txt", "unsolved-2x2-white-layer-20260203_130839.txt")
  //trainRL2x2YellowLayer("solved-2x2-white-layer-1000000-20260203_005055.txt")
  //testRun2x2YellowLayer("q-values-2x2-white-layer-3000000-20260203_141521.txt", "q-values-2x2-yellow-layer-1000000-20260203_143242.txt")
  //trainRL2x2UpperLayer("solved-2x2-yellow-layer-1000000-20260203_010048.txt")
  /*testRun2x2UpperLayer(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt"
  )*/
  //debugUpperLayer("q-values-2x2-upper-layer-10000000-20260203_094831.txt")
  /*solveOneFromScramble(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt",
    "B B L' R B R B' U F' R U D R' B' D"
  )*/

  solveOneFromState(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt",
    //"BFULLRFBFUBDLUDRFRUDLBRD"
    //"UDFLULDUDBRBLFFURRBBLDRF"
    //"UFRBLRRFDDBDULRLBFFLDUBU"
    "LDRFFBBUDLLUBBRDULURFDRF"
  )

  /*solveRandomOne(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt"
  )*/
}


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
    val action = agent.nextBestTrainingAction(e, 0.001)
    if (!e.isSolved) e.step(action) else e
  )

@tailrec
def iteration(agent: Agent, envGenerator: () => Environment, toGo: Long, initialMax: Long, maxMoves: Int,
              epochEpisodes: Long, successThisEpoch: Long): Agent =
  if (toGo == 0) agent
  else
    val initEnv = envGenerator()
    val afterEnvironment = episode(agent, initEnv, maxMoves)
    /*if(!afterEnvironment.isSolved) {
      println("------------")
      println(f"${initEnv.history.mkString("\n")}")
      println("------------")
    }*/
    var nextSuccessCounter = if(afterEnvironment.isSolved) successThisEpoch + 1 else successThisEpoch
    if(toGo % epochEpisodes == 0)
      printAgentStats(agent, initialMax - toGo)
      println(f"epoch: ${EpochLog(epochEpisodes, nextSuccessCounter).toString}")
      nextSuccessCounter = 0
    iteration(agent.updateEpisode(afterEnvironment), envGenerator, toGo - 1, initialMax, maxMoves, epochEpisodes, nextSuccessCounter)

def trainRL2x2WhiteLayer(): Unit =
  val start = LocalDateTime.now()
  println(start)
  val max = 3000000
  val epochEpisodes = 50000
  val episodeMoves = 50
  val agent = Agent(0.25, 0.05, 2000L) // The most random and exploratory
  val afterAgent = iteration(agent, () => Environment.init2x2WhiteLayerTraining(1 + scala.util.Random.nextInt(50)), max, max, episodeMoves, epochEpisodes, 0)
  printAgentStats(afterAgent, max)
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  println("Saving q-values to file")
  afterAgent.saveQState(f"q-values-2x2-white-layer-$max-$timestampTxt.txt")
  afterAgent.saveSolvedStates(f"solved-2x2-white-layer-$max-$timestampTxt.txt")
  val end = LocalDateTime.now()
  println(end)
  val diffSec = start.until(end, ChronoUnit.SECONDS)
  println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

def testRun2x2WhiteLayer(filePath: String): String =
  println(f"Test run with: $filePath")
  val agent = Agent.load(filePath)
  println(f"Loaded: ${agent.qState.keys.size} records")
  val res = mutable.Map[Int, Int]()
  val resUnsolved = mutable.Map[String, Int]()
  (0 until 100000).foreach(e =>
    if(e % 10000 == 0) println(e)
    val env = Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))
    val initMaskedState = env.cube.maskedState
    val initState = env.cube.state
    testRunStage(agent, env, 200, 0.005, 0.02)
    if(env.isSolved) res.update(env.history.length, res.getOrElse(env.history.length, 0) + 1)
    else {
      val unsolvedState = f"$initState|$initMaskedState|${env.cube.state}|${env.cube.maskedState}|${env.initScramble.mkString(" ")}"
      resUnsolved.update(unsolvedState, resUnsolved.getOrElse(unsolvedState, 0) + 1)
      res.update(-1, res.getOrElse(-1, 0) + 1)
    }
  )
  println(res.toVector.sortBy(_._1).mkString("\n"))
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  println("Saving unsolved states to file")
  println(resUnsolved.toVector.sortBy(_._2).reverse.mkString("\n"))
  val fileUS = f"unsolved-2x2-white-layer-$timestampTxt.txt"
  val pw = new PrintWriter(fileUS)
  pw.println(resUnsolved.toVector.map(_._1).mkString("\n"))
  pw.close()
  fileUS

def reTrainRL2x2WhiteLayer(whiteLayerFilePath: String, unsolvedFilePath: String): (String, String) =
  val start = LocalDateTime.now()
  println(start)
  val max = 500000
  val epochEpisodes = 50000
  val episodeMoves = 30
  val agent = Agent.load(whiteLayerFilePath, 0.25, 0.05, 2000L)
  //val agent = Agent(0.25, 0.05, 2000L)
  val unsolvedStates = loadSimple(unsolvedFilePath).map(_.split('|')).map({case Array(initS, initMS, unS, unMS, scr) =>
    (initS, initMS, unS, unMS, scr.split(" ").map(Moves2x2.from).toVector)
  }) //.filter(_._1 == "BLRRDDBUUBBLDFDULLRFFFUR")

  def prepareCube(): Cube =
    val entry = unsolvedStates(scala.util.Random.nextInt(unsolvedStates.length))
    val cube = Cube2x2.solved.applyMask(Environment.whiteLayer2x2Selector)
    val scramble = entry._5
    val scrambledCube = scramble.foldLeft(cube)((c, m) => m.applyToCube(c))
    //println(scrambledCube.state)
    //println(scrambledCube.maskedState)
    scrambledCube

  prepareCube()

  val afterAgent = iteration(agent, () => Environment.init(() => prepareCube(), Environment.whiteLayer2x2ExpectedState),
    max, max, episodeMoves, epochEpisodes, 0)
  printAgentStats(afterAgent, max)
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  println("Saving q-values to file")
  val fileQ = f"re-q-values-2x2-white-layer-$max-$timestampTxt.txt"
  afterAgent.saveQState(fileQ)
  val fileS = f"solved-2x2-white-layer-$max-$timestampTxt.txt"
  afterAgent.saveSolvedStates(fileS)
  val end = LocalDateTime.now()
  println(end)
  val diffSec = start.until(end, ChronoUnit.SECONDS)
  println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")
  (fileQ, fileS)

// retrain multiple times until no unsolved state is discovered
@tailrec
def reTrainAndTestRL2x2WhiteLayer(whiteLayerFilePath: String, unsolvedFilePath: String, counter: Int = 0): Unit =
  val unsolved = loadSimple(unsolvedFilePath)
  if(unsolved.isEmpty || counter == 10) ()
  else
    val (fileQ, _) = reTrainRL2x2WhiteLayer(whiteLayerFilePath, unsolvedFilePath)
    println(f"Retraining with: $fileQ")
    val fileUS = testRun2x2WhiteLayer(fileQ)
    reTrainAndTestRL2x2WhiteLayer(fileQ, fileUS, counter + 1)


def trainRL2x2YellowLayer(whileLayerSolvedFilePath: String): Unit =
  val start = LocalDateTime.now()
  println(start)
  val max = 1000000
  val epochEpisodes = 50000
  val episodeMoves = 100
  val solvedStates = loadSimple(whileLayerSolvedFilePath) // load solved states for white layer to begin with

  def prepareCube(): Cube =
    val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
    Cube2x2(solvedState).applyMask(Environment.yellowLayer2x2Selector)

  val agent = Agent(0.20, 0.01, 1000L)
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


def loadSimple(filePath: String): Vector[String] =
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
    val initWhiteLayerEnv = Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))
    val initWhiteState = initWhiteLayerEnv.cube.state
    // NOTE: we add some randomness to overcome unsolvable states (loops)
    val envWhiteLayer = testRunStage(whiteLayerAgent, initWhiteLayerEnv, 300, 0.005, 0.02)
    if(envWhiteLayer.isSolved)
      val initYellowState = envWhiteLayer.cube.state
      val initYellowLayerEnv = Environment.init(() => envWhiteLayer.cube.applyMask(Environment.yellowLayer2x2Selector), Environment.yellowLayer2x2ExpectedState)
      val envYellowLayer = testRunStage(yellowLayerAgent, initYellowLayerEnv, 200, 0.001)
      if(envYellowLayer.isSolved)
        val length = envWhiteLayer.history.length + envYellowLayer.history.length
        res.update(length, res.getOrElse(length, 0) + 1)
      else res.update(-2, res.getOrElse(-2, 0) + 1)
    else res.update(-1, res.getOrElse(-1, 0) + 1)
  )
  println(res.toVector.sortBy(_._1).mkString("\n"))

def trainRL2x2UpperLayer(yellowLayerSolvedFilePath: String): Unit =
  val start = LocalDateTime.now()
  println(start)
  val max = 10000000 // NOTE: the learning process is not linear in this case as the model needs to find patterns
  val epochEpisodes = 50000
  val episodeMoves = 30
  val solvedStates = loadSimple(yellowLayerSolvedFilePath) // load solved states for yellow layer to begin with

  def prepareCube(): Cube =
    val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
    Cube2x2(solvedState).applyMask(Environment.final2x2Selector)

  val agent = Agent(0.2, 0.001, 10000L) // minimum epsilon - we are looking for pattern, not for different ways to solve the upper layer
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

  val res = mutable.Map[Int, Int]()
  val resUnsolved = mutable.Map[String, Int]()
  val resSolved = mutable.Map[String, Int]()
  (0 until 100000).foreach(e =>
    if(e % 10000 == 0) println(f"$e solved: ${resSolved.values.sum} unsolved: ${resUnsolved.values.sum}")
    val initWhiteLayerEnv = Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))
    val initWhiteState = initWhiteLayerEnv.cube.state
    // NOTE: we add some randomness to overcome unsolvable states (loops)
    val envWhiteLayer = testRunStage(whiteLayerAgent, initWhiteLayerEnv, 300, 0.005, 0.02)
    if(envWhiteLayer.isSolved)
      val initYellowState = envWhiteLayer.cube.state
      val initYellowLayerEnv = Environment.init(() => envWhiteLayer.cube.applyMask(Environment.yellowLayer2x2Selector), Environment.yellowLayer2x2ExpectedState)
      val envYellowLayer = testRunStage(yellowLayerAgent, initYellowLayerEnv, 200, 0.001)
      if(envYellowLayer.isSolved)
        val initUpperState = envYellowLayer.cube.state
        val initUpperLayerEnv = Environment.init(() => envYellowLayer.cube.applyMask(Environment.final2x2Selector), Environment.final2x2ExpectedState)
        val envUpperLayer = testRunStage(upperLayerAgent, initUpperLayerEnv, 200, 0.001)
        if (envUpperLayer.isSolved)
          val length = envWhiteLayer.history.length + envYellowLayer.history.length + envUpperLayer.history.length
          resSolved.update(initUpperState, resSolved.getOrElse(initUpperState, 0) + 1)
          res.update(length, res.getOrElse(length, 0) + 1)
        else
          resUnsolved.update(initUpperState, resUnsolved.getOrElse(initUpperState, 0) + 1)
          res.update(-3, res.getOrElse(-3, 0) + 1)
      else
        resUnsolved.update(initYellowState, resUnsolved.getOrElse(initYellowState, 0) + 1)
        res.update(-2, res.getOrElse(-2, 0) + 1)
    else
      resUnsolved.update(initWhiteState, resUnsolved.getOrElse(initWhiteState, 0) + 1)
      res.update(-1, res.getOrElse(-1, 0) + 1)
  )
  println(res.toVector.sortBy(_._1).mkString("\n"))
  /*println("Unsolved")
  println(resUnsolved.toVector.sortBy(_._2).reverse.mkString("\n"))
  println("Solved")
  println(resSolved.toVector.sortBy(_._2).reverse.mkString("\n"))*/

def testRunStage(agent: Agent, environment: Environment, steps: Int, scale: Double = 1.0, epsilon: Double = 0.0): Environment =
  (0 until steps).foreach(_ => if (!environment.isSolved) environment.step(agent.nextBestAction(environment, scale, epsilon)))
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

def solveOneFromScramble(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String,
                         initialScramble: String): Unit =
  val startSolved = Cube2x2.solved
  println(startSolved.printableState)
  val initialCube = initialScramble.split(" ").map(Moves2x2.from).foldLeft(Cube2x2.solvedWithMask(Environment.whiteLayer2x2Selector))((c, m) =>
    println(f"scramble move: ${m.symbol}")
    val res = m.applyToCube(c)
    println(res.printableState)
    res
  )
  println(f"initial scramble: $initialScramble initial state: ${initialCube.state}")
  println(f"initial:\n${initialCube.printableState}\n")
  solveOne(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, initialCube)

def solveOneFromState(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String,
                         initialState: String): Unit =
  val initialCube = Cube2x2.maskUpperCorners(Cube2x2(initialState))
  solveOne(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, initialCube)


def solveOne(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String,
             initialCube: Cube): Unit =
  println(f"Test run with: $filePathWhiteLayer, $filePathYellowLayer, $filePathUpperLayer")
  val whiteLayerAgent = Agent.load(filePathWhiteLayer)
  println(f"Loaded: ${whiteLayerAgent.qState.keys.size} records (white layer)")
  val yellowLayerAgent = Agent.load(filePathYellowLayer)
  println(f"Loaded: ${yellowLayerAgent.qState.keys.size} records (yellow layer)")
  val upperLayerAgent = Agent.load(filePathUpperLayer)
  println(f"Loaded: ${upperLayerAgent.qState.keys.size} records (upper layer)")

  val initWhiteLayerEnv = Environment.init(() => initialCube, Environment.whiteLayer2x2ExpectedState)
  val initWhiteState = initWhiteLayerEnv.cube.state
  // NOTE: we add some randomness to overcome unsolvable states (loops)
  println("white layer")
  val envWhiteLayer = solveStage(whiteLayerAgent, initWhiteLayerEnv, 300, 0.005, 0.02)
  if (envWhiteLayer.isSolved)
    val initYellowState = envWhiteLayer.cube.state
    val initYellowLayerEnv = Environment.init(() => envWhiteLayer.cube.applyMask(Environment.yellowLayer2x2Selector), Environment.yellowLayer2x2ExpectedState)
    println("yellow layer")
    val envYellowLayer = solveStage(yellowLayerAgent, initYellowLayerEnv, 200, 0.001)
    if (envYellowLayer.isSolved)
      val initUpperState = envYellowLayer.cube.state
      val initUpperLayerEnv = Environment.init(() => envYellowLayer.cube.applyMask(Environment.final2x2Selector), Environment.final2x2ExpectedState)
      println("upper layer")
      val envUpperLayer = solveStage(upperLayerAgent, initUpperLayerEnv, 200, 0.001)
      if (envUpperLayer.isSolved)
        val length = envWhiteLayer.history.length + envYellowLayer.history.length + envUpperLayer.history.length
        println(f"Solved in $length steps")
  println("END")

def solveStage(agent: Agent, environment: Environment, steps: Int, scale: Double = 1.0, epsilon: Double = 0.0): Environment =
  println(f"initial state: \n\n${environment.cube.printableState}\n")
  (0 until steps).foreach(_ =>
    if (!environment.isSolved) {
      val stateBefore = environment.cube.state
      val stateBeforeP = environment.cube.printableState
      val action = agent.nextBestAction(environment, scale, epsilon)
      environment.step(action)
      val stateAfter = environment.cube.state
      val stateAfterP = environment.cube.printableState

      //println(f"before: $stateBefore action: $action after: $stateAfter")
      println(f"\naction: $action -> \n\n$stateAfterP")
    })
  environment

def solveRandomOne(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String): Unit =
  val moves = Moves2x2.randomList(scala.util.Random.nextInt(5) + 10).map(_.symbol).mkString(" ")
  solveOneFromScramble(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, moves)