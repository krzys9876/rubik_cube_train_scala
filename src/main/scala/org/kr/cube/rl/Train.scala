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

  def loadAgents(files: Vector[AgentFile]): Vector[StageSpec] =
    def loadOneAgent(file: AgentFile): Agent =
      val agent = Agent.load(file.filePath)
      println(f"Loaded: ${agent.qState.keys.size} records (${file.label}) from ${file.filePath}")
      agent

    files.map(f => StageSpec(f, loadOneAgent(f)))

  def testRunStage(agent: Agent, environment: Environment, config: StageConfig,
                   printState: Boolean = false): Environment =
    (0 until config.moves).foreach(_ =>
      if (!environment.isSolved)
        environment.step(agent.nextBestAction(environment, config.precision, config.epsilon), printState))
    environment

  def loadSimple(filePath: String): Vector[String] =
    val source = scala.io.Source.fromFile(filePath)
    try
      source.getLines().toVector
    finally source.close()


  def startSolving(stages: Vector[StageSpec], envGenerator: Map[String, String => Environment],
                   stageConfig: Map[String, StageConfig], 
                   steps: Int = 100000, debugEvery: Int = 10000, printState: Boolean = false): TestRunResults =
    val results = TestRunResults.empty
    (0 until steps).foreach(e =>
      if (e > 0 && (e % debugEvery == 0)) println(f"$e solved: ${results.resSolved.values.sum} unsolved: ${results.resUnsolved.values.sum}")
      solveNextStage("X", stages, Some(results), 0, false, envGenerator, stageConfig)
    )
    results

  
  @tailrec
  def solveNextStage(initState: String, stages: Vector[StageSpec],
                     res: Option[TestRunResults], prevLength: Int, printState: Boolean,
                     envGenerator: Map[String, String => Environment],
                     stageConfig: Map[String, StageConfig]): Environment =
    if (printState) println(stages.head.file.label)
    val env = envGenerator(stages.head.file.label)(initState)
    val startState = env.cube.state
    val agent = stages.head.agent
    val envAfter = Train.testRunStage(agent, env, stageConfig(stages.head.file.label), printState)
    if (envAfter.isSolved) {
      val remainingAgents = stages.tail
      val newLength = prevLength + envAfter.history.length
      if (remainingAgents.nonEmpty) 
        solveNextStage(envAfter.cube.state, remainingAgents, res, newLength, printState, envGenerator, stageConfig)
      else
        if (res.isDefined)
          res.get.resSolved.update(initState, res.get.resSolved.getOrElse(initState, 0) + 1)
          res.get.res.update(newLength, res.get.res.getOrElse(newLength, 0) + 1)
        envAfter
    } else
      if (res.isDefined)
        res.get.resUnsolved.update(startState, res.get.resUnsolved.getOrElse(startState, 0) + 1)
        res.get.res.update(-1, res.get.res.getOrElse(-1, 0) + 1)
      envAfter


case class AgentFile(filePath: String, label: String)

case class TestRunResults(res: mutable.Map[Int, Int], resSolved: mutable.Map[String, Int], resUnsolved: mutable.Map[String, Int])

object TestRunResults:
  def empty: TestRunResults =
    TestRunResults(mutable.Map[Int, Int](), mutable.Map[String, Int](), mutable.Map[String, Int]())

case class StageSpec(file: AgentFile, agent: Agent)

case class StageConfig(moves: Int, precision: Double, epsilon: Double)