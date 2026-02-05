package org.kr.cube.rl

import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable

object Train:
  // NOTE: having low maximum number of moves (say 25) and high maximum iterations increases model accuracy
  @tailrec
  private def episode(agent: Agent, env: Environment, moves: Int): Environment =
    if(env.success || moves == 0) env
    else episode(agent, env.trainingStep(agent), moves - 1)

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
      var nextSuccessCounter = if (afterEnvironment.success) successThisEpoch + 1 else successThisEpoch
      if (toGo % epochEpisodes == 0)
        agent.printStats(initialMax - toGo)
        println(f"epoch: ${EpochLog(epochEpisodes, nextSuccessCounter).toString}")
        nextSuccessCounter = 0
      iteration(agent.updateEpisode(afterEnvironment), envGenerator, toGo - 1, initialMax, maxMoves, epochEpisodes, nextSuccessCounter)

  def loadAgents(filePaths: Vector[(String, String)]): Vector[Agent] =
    def loadOneAgent(filePath: String, label: String): Agent =
      val agent = Agent.load(filePath)
      println(f"Loaded: ${agent.qState.keys.size} records ($label) from $filePath")
      agent
    
    filePaths.map({case(file, label) => loadOneAgent(file, label)})

  def createTestRunResults(): (mutable.Map[Int, Int], mutable.Map[String, Int], mutable.Map[String, Int]) =
    (mutable.Map[Int, Int](), mutable.Map[String, Int](), mutable.Map[String, Int]())

  def testRunStage(agent: Agent, environment: Environment, steps: Int, scale: Double = 1.0, epsilon: Double = 0.0,
                   printState: Boolean = false): Environment =
    (0 until steps).foreach(_ =>
      if (!environment.isSolved)
        environment.step(agent.nextBestAction(environment, scale, epsilon), printState))
    environment

  def loadSimple(filePath: String): Vector[String] =
    val source = scala.io.Source.fromFile(filePath)
    try
      source.getLines().toVector
    finally source.close()
