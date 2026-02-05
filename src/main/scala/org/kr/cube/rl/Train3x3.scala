package org.kr.cube.rl

import org.kr.cube.{Cube, Cube3x3}

import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.collection.mutable

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
    val (agent, _, _) = Train.loadAgents(Some(filePath), None, None)
    val (res, resUnsolved, resSolved) = Train.createTestRunResults()
    (0 until 100000).foreach(e =>
      if (e % 10000 == 0) println(f"$e solved: ${resSolved.values.sum} unsolved: ${resUnsolved.values.sum}")
      val env = Environment.init3x3WhiteCrossTraining(10 + scala.util.Random.nextInt(20))
      whiteCrossStage(env, agent.get, None, Some(res), Some(resUnsolved), Some(resSolved))
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

  private def whiteCrossStage(initWhiteCrossEnv: Environment, whiteCrossAgent: Agent, whiteLayerLAgent: Option[Agent], /*upperLayerAgent: Option[Agent],*/
                              res: Option[mutable.Map[Int, Int]],
                              resUnsolved: Option[mutable.Map[String, Int]],
                              resSolved: Option[mutable.Map[String, Int]],
                              printState: Boolean = false): Environment =
    val initWhiteCrossState = initWhiteCrossEnv.cube.state
    // NOTE: we add some randomness to overcome unsolvable states (loops)
    val envWhiteCross = Train.testRunStage(whiteCrossAgent, initWhiteCrossEnv, 300, 0.001, 0.01, printState)
    if (envWhiteCross.isSolved)
      if (whiteLayerLAgent.isDefined) whiteLayerLStage(envWhiteCross, whiteLayerLAgent.get, /* upperLayerAgent,*/
        res, resUnsolved, resSolved, envWhiteCross.history.length)
      else
        if (resSolved.isDefined) resSolved.get.update(initWhiteCrossState, resSolved.get.getOrElse(initWhiteCrossState, 0) + 1)
        if (res.isDefined) res.get.update(envWhiteCross.history.length, res.get.getOrElse(envWhiteCross.history.length, 0) + 1)
        envWhiteCross
    else
      if (resUnsolved.isDefined) resUnsolved.get.update(initWhiteCrossState, resUnsolved.get.getOrElse(initWhiteCrossState, 0) + 1)
      if (res.isDefined) res.get.update(-1, res.get.getOrElse(-1, 0) + 1)
      envWhiteCross

  def trainRL3x3WhiteLayer(whiteCrossSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 3000000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteCrossSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      //val solvedState = Cube3x3.solved.state
      val c = Cube3x3.maskUpperLayers(Cube3x3(solvedState))
      //println(c.state)
      //println(c.maskedState)
      c

    val agent = Agent(0.25, 0.01, 6000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  private def whiteLayerLStage(initWhiteLayerLEnv: Environment, whiteLayerLAgent: Agent /*, yellowLayerAgent: Option[Agent], upperLayerAgent: Option[Agent]*/ ,
                               res: Option[mutable.Map[Int, Int]],
                               resUnsolved: Option[mutable.Map[String, Int]],
                               resSolved: Option[mutable.Map[String, Int]],
                               prevLength: Int, printState: Boolean = false): Environment =
    val initWhiteLayerLState = initWhiteLayerLEnv.cube.state
    // NOTE: we add some randomness to overcome unsolvable states (loops)
    val envWhiteLayerL = Train.testRunStage(whiteLayerLAgent, initWhiteLayerLEnv, 300, 0.001, 0.01, printState)
    if (envWhiteLayerL.isSolved)
      /*if (yellowLayerAgent.isDefined) yellowLayerStage(envWhiteLayer.cube.state, yellowLayerAgent.get, upperLayerAgent,
        res, resUnsolved, resSolved, envWhiteLayer.history.length)
      else*/
      if (resSolved.isDefined) resSolved.get.update(initWhiteLayerLState, resSolved.get.getOrElse(initWhiteLayerLState, 0) + 1)
      if (res.isDefined) res.get.update(prevLength + envWhiteLayerL.history.length, res.get.getOrElse(prevLength + envWhiteLayerL.history.length, 0) + 1)
      envWhiteLayerL
    else
      if (resUnsolved.isDefined) resUnsolved.get.update(initWhiteLayerLState, resUnsolved.get.getOrElse(initWhiteLayerLState, 0) + 1)
      if (res.isDefined) res.get.update(-1, res.get.getOrElse(-1, 0) + 1)
      envWhiteLayerL

  def testRun3x3WhiteLayerL(filePathWhiteCross: String, filePathWhiteLayerL: String): String =
    val (agentWhiteCross, agetnWhileLayerL, _) = Train.loadAgents(Some(filePathWhiteCross), Some(filePathWhiteLayerL), None)
    val (res, resUnsolved, resSolved) = Train.createTestRunResults()
    (0 until 100000).foreach(e =>
      if (e % 10000 == 0) println(f"$e solved: ${resSolved.values.sum} unsolved: ${resUnsolved.values.sum}")
      val env = Environment.init3x3WhiteCrossTraining(10 + scala.util.Random.nextInt(20))
      whiteCrossStage(env, agentWhiteCross.get, agetnWhileLayerL, Some(res), Some(resUnsolved), Some(resSolved))
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
