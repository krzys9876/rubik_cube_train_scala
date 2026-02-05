package org.kr.cube.rl

import org.kr.cube.rl.Train.{startSolving, startSolvingSingle}
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
    val stages = Train.loadAgents(Vector(AgentFile(filePath,"white layer")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving unsolved states to file")
    println(results.resUnsolved.toVector.sortBy(_._2).reverse.mkString("\n"))
    val fileUS = f"unsolved-2x2-white-layer-$timestampTxt.txt"
    val pw = new PrintWriter(fileUS)
    pw.println(results.resUnsolved.toVector.map(_._1).mkString("\n"))
    pw.close()
    fileUS


  def trainRL2x2YellowLayer(whileLayerSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 2000000
    val epochEpisodes = 50000
    val episodeMoves = 100
    val solvedStates = Train.loadSimple(whileLayerSolvedFilePath) // load solved states for white layer to begin with

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


  def testRun2x2YellowLayer(filePathWhiteLayer: String, filePathYellowLayer: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteLayer,"white layer"), AgentFile(filePathYellowLayer, "yellow layer")))
    val results = startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))

  def trainRL2x2UpperLayer(yellowLayerSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 10000000 // NOTE: the learning process is not linear in this case as the model needs to find patterns
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(yellowLayerSolvedFilePath) // load solved states for yellow layer to begin with

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
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteLayer,"white layer"),
      AgentFile(filePathYellowLayer, "yellow layer"), AgentFile(filePathUpperLayer, "upper layer")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))
  /*println("Unsolved")
  println(resUnsolved.toVector.sortBy(_._2).reverse.mkString("\n"))
  println("Solved")
  println(resSolved.toVector.sortBy(_._2).reverse.mkString("\n"))*/


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
    solveOne(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, initialCube)

  def solveOneFromState(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String,
                        initialState: String): Unit =
    val initialCube = Cube2x2.maskUpperCorners(Cube2x2(initialState))
    println(f"initial state: ${initialCube.state}")
    solveOne(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, initialCube)

  def solveRandomOne(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String): Unit =
    val moves = Moves2x2.randomList(scala.util.Random.nextInt(5) + 10).map(_.symbol).mkString(" ")
    solveOneFromScramble(filePathWhiteLayer, filePathYellowLayer, filePathUpperLayer, moves)

  def solveOne(filePathWhiteLayer: String, filePathYellowLayer: String, filePathUpperLayer: String,
               initialCube: Cube): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteLayer,"white layer"),
      AgentFile(filePathYellowLayer, "yellow layer"), AgentFile(filePathUpperLayer, "upper layer")))
    println(f"initial:\n${initialCube.printableState}\n")
    val results = Train.startSolvingSingle(initialCube.state, stages, envGeneratorSingle, stageConfig, true)
    val moves = results.res.toVector.map(_._1).head
    if(moves < 0) println("Unsolved") else println(f"Solved in $moves moves")


  // Random initial state
  val envGenerator: Map[String, String => Environment] = Map(
    "white layer" -> (_ => Environment.init2x2WhiteLayerTraining(10 + scala.util.Random.nextInt(20))),
    "yellow layer" -> (state => Environment.init2x2(() => Cube2x2.maskUpperLayer(Cube2x2(state)), Environment.yellowLayer2x2ExpectedState)),
    "upper layer" -> (state => Environment.init2x2(() => Cube2x2(state), Environment.final2x2ExpectedState))
  )

  // Given initial state
  val envGeneratorStartState: Map[String, String => Environment] = Map(
    "white layer" -> (state => Environment.init2x2(() => Cube2x2.maskUpperCorners(Cube2x2(state)), Environment.whiteLayer2x2ExpectedState))
  )

  // Full stage config for given initial state
  val envGeneratorSingle: Map[String, String => Environment] = envGenerator ++ envGeneratorStartState

  // Parameters for testRunStage (moves, precision, epsilon)
  val stageConfig: Map[String, StageConfig] = Map(
    "white layer" -> StageConfig(200, 0.001, 0.0),
    "yellow layer" -> StageConfig(200, 0.001, 0.0),
    "upper layer" -> StageConfig(200, 0.001, 0.0)
  )