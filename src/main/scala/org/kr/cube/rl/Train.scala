package org.kr.cube.rl

import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable

object Train:
  def printAgentStats(agent: Agent, max: Long): Unit =
    println(LocalDateTime.now())
    val agg = agent.qState.groupBy({ case (_, (c, _)) => c }).map((c, entries) => c -> entries.size).toVector.sortBy(_._1).reverse
    val singleVisited = agg.filter(_._1 == 1).map(_._2).sum
    val allOther = agg.filter(_._1 > 1).map(_._2).sum
    //println(f"q-values stats (number of visits - number of states): \n${agg.mkString("\n")}")
    println(f"q-values: ${agent.qState.keys.size} keys, single visits: $singleVisited, other: $allOther, ratio ${singleVisited.toDouble / (singleVisited + allOther).toDouble * 100.0}%.3f%%")
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
      var nextSuccessCounter = if (afterEnvironment.isSolved) successThisEpoch + 1 else successThisEpoch
      if (toGo % epochEpisodes == 0)
        printAgentStats(agent, initialMax - toGo)
        println(f"epoch: ${EpochLog(epochEpisodes, nextSuccessCounter).toString}")
        nextSuccessCounter = 0
      iteration(agent.updateEpisode(afterEnvironment), envGenerator, toGo - 1, initialMax, maxMoves, epochEpisodes, nextSuccessCounter)

  def loadAgents(filePathWhiteLayer: Option[String], filePathYellowLayer: Option[String],
                 filePathUpperLayer: Option[String]): (Option[Agent], Option[Agent], Option[Agent]) =
    def loadOneAgent(filePath: String, label: String): Agent =
      val agent = Agent.load(filePath)
      println(f"Loaded: ${agent.qState.keys.size} records ($label) from $filePath")
      agent

    val whiteLayerAgent = filePathWhiteLayer.map(f => loadOneAgent(f, "white layer"))
    val yellowLayerAgent = filePathYellowLayer.map(f => loadOneAgent(f, "yellow layer"))
    val upperLayerAgent = filePathUpperLayer.map(f => loadOneAgent(f, "upper layer"))
    (whiteLayerAgent, yellowLayerAgent, upperLayerAgent)

  def createTestRunResults(): (mutable.Map[Int, Int], mutable.Map[String, Int], mutable.Map[String, Int]) =
    (mutable.Map[Int, Int](), mutable.Map[String, Int](), mutable.Map[String, Int]())

  def testRunStage(agent: Agent, environment: Environment, steps: Int, scale: Double = 1.0, epsilon: Double = 0.0): Environment =
    (0 until steps).foreach(_ => if (!environment.isSolved) environment.step(agent.nextBestAction(environment, scale, epsilon)))
    environment
