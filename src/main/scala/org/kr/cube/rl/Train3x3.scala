package org.kr.cube.rl

import org.kr.cube.{Cube, Cube3x3}

import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object Train3x3:
  def trainRL3x3WhiteCross(): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 1000000
    val epochEpisodes = 50000
    val episodeMoves = 50
    val agent = Agent(0.25, 0.02, 2000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3WhiteCrossTraining(1 + scala.util.Random.nextInt(50)), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-cross-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-cross-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

  def testRun3x3WhiteCross(filePath: String): String =
    val stages = Train.loadAgents(Vector(AgentFile(filePath, "white cross")))
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


  def trainRL3x3WhiteLayerFL(whiteCrossSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 1000000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteCrossSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      //val solvedState = Cube3x3.solved.state
      val c = Cube3x3.maskUpperLayersFL(Cube3x3(solvedState))
      //println(c.state)
      //println(c.maskedState)
      c

    val agent = Agent(0.25, 0.01, 5000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3FLExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-l-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-l-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3WhiteLayerLF(filePathWhiteCross: String, filePathWhiteLayerL: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross,"white cross"), AgentFile(filePathWhiteLayerL, "white layer (0)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  val envGenerator: Map[String, String => Environment] = Map(
    "white cross" -> (_ => Environment.init3x3WhiteCrossTraining(10 + scala.util.Random.nextInt(20))),
    "white layer (FL)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersFL(Cube3x3(state)), Environment.whiteLayer3x3FLExpectedState)),
    "white layer (LB)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersLB(Cube3x3(state)), Environment.whiteLayer3x3LBExpectedState)),
    "white layer (BR)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersBR(Cube3x3(state)), Environment.whiteLayer3x3BRExpectedState)),
    "white layer (all)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersAll(Cube3x3(state)), Environment.whiteLayer3x3AllExpectedState))
  )

  // Parameters for testRunStage (moves, precision, epsilon)
  val stageConfig: Map[String, StageConfig] = Map(
    "white cross" -> StageConfig(100, 0.001, 0.0), // No randomness
    "white layer (FL)" -> StageConfig(100, 0.001, 0.0),
    "white layer (LB)" -> StageConfig(100, 0.001, 0.0),
    "white layer (BR)" -> StageConfig(100, 0.001, 0.0),
    "white layer (all)" -> StageConfig(100, 0.001, 0.0)
  )