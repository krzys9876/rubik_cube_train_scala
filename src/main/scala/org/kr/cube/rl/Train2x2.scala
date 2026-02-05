package org.kr.cube.rl

import org.kr.cube.{Cube, Cube2x2, Moves2x2}

import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.collection.mutable

object Train2x2:
  def trainRL2x2WhiteLayer(): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 2000000
    val epochEpisodes = 50000
    val episodeMoves = 50
    val agent = Agent(0.25, 0.02, 5000L) // The most random and exploratory
    val afterAgent = Train.iteration(agent, () => Environment.init2x2WhiteLayerTraining(1 + scala.util.Random.nextInt(50)), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-2x2-white-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-2x2-white-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

  def testRun2x2WhiteLayer(filePath: String): String =
    val (agent, _, _) = Train.loadAgents(Some(filePath), None, None)
    val (res, resUnsolved, resSolved) = Train.createTestRunResults()
    (0 until 100000).foreach(e =>
      if (e % 10000 == 0) println(f"$e solved: ${resSolved.values.sum} unsolved: ${resUnsolved.values.sum}")
      val env = Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))
      whiteLayerStage(env, agent.get, None, None, Some(res), Some(resUnsolved), Some(resSolved))
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


  def trainRL2x2YellowLayer(whileLayerSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 2000000
    val epochEpisodes = 50000
    val episodeMoves = 100
    val solvedStates = loadSimple(whileLayerSolvedFilePath) // load solved states for white layer to begin with

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube2x2.maskUpperLayer(Cube2x2(solvedState))

    val agent = Agent(0.20, 0.005, 2000L)
    val afterAgent = Train.iteration(agent, () => Environment.init2x2(() => prepareCube(), Environment.yellowLayer2x2ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
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
    val (whiteLayerAgent, yellowLayerAgent, _) = Train.loadAgents(Some(filePathWhiteLayer), Some(filePathYellowLayer), None)
    val (res, resUnsolved, resSolved) = Train.createTestRunResults()
    (0 until 100000).foreach(e =>
      if (e % 10000 == 0) println(f"$e solved: ${resSolved.values.sum} unsolved: ${resUnsolved.values.sum}")
      val initWhiteLayerEnv = Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))
      whiteLayerStage(initWhiteLayerEnv, whiteLayerAgent.get, yellowLayerAgent, None, Some(res), Some(resUnsolved), Some(resSolved))
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
      Cube2x2(solvedState)

    val agent = Agent(0.2, 0.005, 10000L) // minimum epsilon - we are looking for pattern, not for different ways to solve the upper layer
    val afterAgent = Train.iteration(agent, () => Environment.init2x2(() => prepareCube(), Environment.final2x2ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-2x2-upper-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-2x2-upper-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun2x2UpperLayer(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String): Unit =
    val (whiteLayerAgent, yellowLayerAgent, upperLayerAgent) = Train.loadAgents(Some(filePathWhiteLayer), Some(filePathYellowLayer), Some(filePathUpperLayer))
    val (res, resUnsolved, resSolved) = Train.createTestRunResults()
    (0 until 100000).foreach(e =>
      if (e % 10000 == 0) println(f"$e solved: ${resSolved.values.sum} unsolved: ${resUnsolved.values.sum}")
      val initWhiteLayerEnv = Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))
      whiteLayerStage(initWhiteLayerEnv, whiteLayerAgent.get, yellowLayerAgent, upperLayerAgent,
        Some(res), Some(resUnsolved), Some(resSolved))
    )
    println(res.toVector.sortBy(_._1).mkString("\n"))
  /*println("Unsolved")
  println(resUnsolved.toVector.sortBy(_._2).reverse.mkString("\n"))
  println("Solved")
  println(resSolved.toVector.sortBy(_._2).reverse.mkString("\n"))*/

  private def whiteLayerStage(initWhiteLayerEnv: Environment, whiteLayerAgent: Agent, yellowLayerAgent: Option[Agent], upperLayerAgent: Option[Agent],
                              res: Option[mutable.Map[Int, Int]],
                              resUnsolved: Option[mutable.Map[String, Int]],
                              resSolved: Option[mutable.Map[String, Int]],
                              printState: Boolean = false): Environment =
    val initWhiteState = initWhiteLayerEnv.cube.state
    // NOTE: we add some randomness to overcome unsolvable states (loops)
    val envWhiteLayer = Train.testRunStage(whiteLayerAgent, initWhiteLayerEnv, 300, 0.001, 0.01, printState)
    if (envWhiteLayer.isSolved)
      if(yellowLayerAgent.isDefined) yellowLayerStage(envWhiteLayer.cube.state, yellowLayerAgent.get, upperLayerAgent,
        res, resUnsolved, resSolved, envWhiteLayer.history.length, printState)
      else
        if(resSolved.isDefined) resSolved.get.update(initWhiteState, resSolved.get.getOrElse(initWhiteState, 0) + 1)
        envWhiteLayer
    else
      if(resUnsolved.isDefined) resUnsolved.get.update(initWhiteState, resUnsolved.get.getOrElse(initWhiteState, 0) + 1)
      if(res.isDefined) res.get.update(-1, res.get.getOrElse(-1, 0) + 1)
      envWhiteLayer

  private def yellowLayerStage(initialState: String, yellowLayerAgent: Agent, upperLayerAgent: Option[Agent],
                               res: Option[mutable.Map[Int, Int]],
                               resUnsolved: Option[mutable.Map[String, Int]],
                               resSolved: Option[mutable.Map[String, Int]],
                               prevLength: Int, printState: Boolean = false): Environment =
    val initYellowLayerEnv = Environment.init2x2(() => Cube2x2.maskUpperLayer(Cube2x2(initialState)), Environment.yellowLayer2x2ExpectedState)
    val envYellowLayer = Train.testRunStage(yellowLayerAgent, initYellowLayerEnv, 200, 0.001, 0.0, printState)
    if (envYellowLayer.isSolved)
      if(upperLayerAgent.isDefined)
        upperLayerStage(envYellowLayer.cube.state, upperLayerAgent.get, res, resUnsolved, resSolved,
        prevLength + envYellowLayer.history.length, printState)
      else
        if(resSolved.isDefined) resSolved.get.update(initialState, resSolved.get.getOrElse(initialState, 0) + 1)
        envYellowLayer
    else
      if(resUnsolved.isDefined) resUnsolved.get.update(initialState, resUnsolved.get.getOrElse(initialState, 0) + 1)
      if(res.isDefined) res.get.update(-2, res.get.getOrElse(-2, 0) + 1)
      envYellowLayer

  private def upperLayerStage(initialState: String, agent: Agent, res: Option[mutable.Map[Int, Int]],
                              resUnsolved: Option[mutable.Map[String, Int]], resSolved: Option[mutable.Map[String, Int]],
                              prevLength: Int, printState: Boolean = false): Environment =
    val initUpperLayerEnv = Environment.init2x2(() => Cube2x2(initialState), Environment.final2x2ExpectedState)
    val envUpperLayer = Train.testRunStage(agent, initUpperLayerEnv, 200, 0.001, 0.0, printState)
    if (envUpperLayer.isSolved)
      val length = prevLength + envUpperLayer.history.length
      if(resSolved.isDefined) resSolved.get.update(initialState, resSolved.get.getOrElse(initialState, 0) + 1)
      if(res.isDefined) res.get.update(length, res.get.getOrElse(length, 0) + 1)
    else
      if(resUnsolved.isDefined) resUnsolved.get.update(initialState, resUnsolved.get.getOrElse(initialState, 0) + 1)
      if(res.isDefined) res.get.update(-3, res.get.getOrElse(-3, 0) + 1)
    envUpperLayer


  def debugUpperLayer(filePathUpperLayer: String): Unit =
    val agent = Agent.load(filePathUpperLayer)
    println(f"Loaded: ${agent.qState.keys.size} records (upper layer)")
    val environment = Environment.init2x2(() => Cube2x2("LRFFFBLLRLBBBFRRUUUUDDDD"), Environment.final2x2ExpectedState)
    val hist = mutable.ArrayBuffer[String]()
    (0 until 100).foreach(_ =>
      if (!environment.isSolved) {
        environment.step(agent.nextBestAction(environment))
        hist.append(f"${environment.history.last.toString}|${environment.history.last.action}|${environment.cube.state}|${environment.isSolved}")
      })
    if (environment.isSolved) println("Solved!")
    println(hist.mkString("\n"))

  def solveOneFromScramble(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String,
                           initialScramble: String): Unit =
    val startSolved = Cube2x2.solved
    println(startSolved.printableState)
    val initialCube = initialScramble.split(" ").map(Moves2x2.from).foldLeft(Cube2x2.maskUpperCorners(Cube2x2.solved2x2))((c, m) =>
      println(f"scramble move: ${m.symbol}")
      val res = m.applyToCube(c)
      println(res.printableState)
      res
    )
    println(f"initial scramble: $initialScramble initial state: ${initialCube.state}")
    println(f"initial:\n${initialCube.printableState}\n")
    //solveOne(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, initialCube)

    val (whiteLayerAgent, yellowLayerAgent, upperLayerAgent) = Train.loadAgents(Some(filePathWhiteLayer), Some(filePathYellowLayer), Some(filePathUpperLayer))
    val (res, resUnsolved, resSolved) = Train.createTestRunResults()
    val initWhiteLayerEnv = Environment.init2x2(() => Cube2x2.maskUpperCorners(Cube2x2(initialCube.state)), Environment.whiteLayer2x2ExpectedState)
    whiteLayerStage(initWhiteLayerEnv, whiteLayerAgent.get, yellowLayerAgent, upperLayerAgent,
        Some(res), Some(resUnsolved), Some(resSolved), true)
    println(res.toVector.sortBy(_._1).mkString("\n"))


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

    val initWhiteLayerEnv = Environment.init2x2(() => initialCube, Environment.whiteLayer2x2ExpectedState)
    val initWhiteState = initWhiteLayerEnv.cube.state
    // NOTE: we add some randomness to overcome unsolvable states (loops)
    println("white layer")
    val envWhiteLayer = solveStage(whiteLayerAgent, initWhiteLayerEnv, 300, 0.001, 0.001)
    if (envWhiteLayer.isSolved)
      val initYellowState = envWhiteLayer.cube.state
      val initYellowLayerEnv = Environment.init2x2(() => Cube2x2.maskUpperLayer(Cube2x2(initYellowState)), Environment.yellowLayer2x2ExpectedState)
      println("yellow layer")
      val envYellowLayer = solveStage(yellowLayerAgent, initYellowLayerEnv, 200, 0.001)
      if (envYellowLayer.isSolved)
        val initUpperState = envYellowLayer.cube.state
        val initUpperLayerEnv = Environment.init2x2(() => envYellowLayer.cube, Environment.final2x2ExpectedState)
        println("upper layer")
        val envUpperLayer = solveStage(upperLayerAgent, initUpperLayerEnv, 200, 0.001)
        if (envUpperLayer.isSolved)
          val length = envWhiteLayer.history.length + envYellowLayer.history.length + envUpperLayer.history.length
          println(f"Solved in $length steps")
    println("END")

  private def solveStage(agent: Agent, environment: Environment, steps: Int, scale: Double = 1.0, epsilon: Double = 0.0): Environment =
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