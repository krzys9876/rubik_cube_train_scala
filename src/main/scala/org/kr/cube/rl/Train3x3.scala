package org.kr.cube.rl

import org.kr.cube.{Cube, Cube3x3, Moves3x3}

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


  def trainRL3x3WhiteLayer2(whiteCrossSolvedFilePath: String, prevQValuesFilePath: Option[String] = None): Unit =
    val start = LocalDateTime.now()
    val epochEpisodes = 50000
    println(start)
    val (agent, max, episodeMoves) =
      if (prevQValuesFilePath.isDefined) (Agent.load(prevQValuesFilePath.get, 0.20, 0.01, 10000L), 10000000, 50)
      else (Agent(0.25, 0.02, 2000L), 5000000, 50)

    val solvedStates = Train.loadSimple(whiteCrossSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayersAll(Cube3x3(solvedState))

    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3AllExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer2-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer2-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def pretrainRL3x3WhiteLayer(solutionsFilePath: String): Unit =
    val solvedStates = Train.loadSimple(solutionsFilePath)
    val decoded = solvedStates.map(_.split('|')).map({ case Array(state, solve, solvedState) => (state, solve, solvedState) })
    println(f"Pretraining using ${decoded.length} solved cubes from $solutionsFilePath")

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = decoded.foldLeft(agent)((a, d) =>
      val environment = Environment.init3x3(() => Cube3x3.maskUpperLayersAll(Cube3x3(d._1)), Environment.whiteLayer3x3AllExpectedState)
      d._2.split(" ").foreach(a => environment.step(a))
      a.updateEpisode(environment)
    )
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-pre-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-pre-$timestampTxt.txt")


  def pretrainRL3x3WhiteLayer2(solutionsFilePath: String): Unit =
    val solvedStates = Train.loadSimple(solutionsFilePath)
    val decoded = solvedStates.map(_.split('|')).map({ case Array(state, solve, solvedState) => (state, solve, solvedState) })
    println(f"Pretraining using ${decoded.length} solved cubes from $solutionsFilePath")

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = decoded.foldLeft(agent)((a, d) =>
      val environment = Environment.init3x3(() => Cube3x3.maskUpperLayersAll(Cube3x3(d._1)), Environment.whiteLayer3x3AllExpectedState)
      d._2.split(" ").foreach(a => environment.step(a))
      a.updateEpisode(environment)
    )
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer2-pre-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer2-pre-$timestampTxt.txt")


  def testRun3x3WhiteLayer2(filePathWhiteCross: String, filePathWhiteLayer2: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayer2, "white layer (all)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


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
    val max = 1500000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteCrossSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayersFL(Cube3x3(solvedState))

    val agent = Agent(0.25, 0.02, 5000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3FLExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-fl-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-fl-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3WhiteLayerFL(filePathWhiteCross: String, filePathWhiteLayerL: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross,"white cross"), AgentFile(filePathWhiteLayerL, "white layer (FL)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))

  def trainRL3x3WhiteLayerLB(whiteLayerFLSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 1500000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteLayerFLSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayersLB(Cube3x3(solvedState))

    val agent = Agent(0.25, 0.02, 5000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3LBExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-lb-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-lb-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3WhiteLayerLB(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3WhiteLayerBR(whiteLayerLBSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 1500000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteLayerLBSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayersBR(Cube3x3(solvedState))

    val agent = Agent(0.25, 0.02, 5000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3BRExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-br-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-br-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3WhiteLayerBR(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                             filePathWhiteLayerBR: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3WhiteLayerAll(whiteLayerBRSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 1500000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteLayerBRSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayersAll(Cube3x3(solvedState))

    val agent = Agent(0.25, 0.02, 5000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.whiteLayer3x3AllExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3WhiteLayerAll(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                             filePathWhiteLayerBR: String, filePathWhiteLayerAll: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3MiddleLayerFL(whiteLayerAllSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 5000000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(whiteLayerAllSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayerFL(Cube3x3(solvedState))

    val agent = Agent(0.20, 0.02, 5000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.middleLayer3x3FLExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-middle-layer-fl-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-middle-layer-fl-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3MiddleLayerFL(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                              filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                              filePathMidLayerFL: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3MiddleLayerLB(midLayerFLSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 5000000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(midLayerFLSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayerLB(Cube3x3(solvedState))

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.middleLayer3x3LBExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-middle-layer-lb-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-middle-layer-lb-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

  def testRun3x3MiddleLayerLB(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                              filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                              filePathMidLayerFL: String, filePathMidLayerLB: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3MiddleLayerBR(midLayerLBSolvedFilePath: String): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 5000000
    val epochEpisodes = 50000
    val episodeMoves = 30
    val solvedStates = Train.loadSimple(midLayerLBSolvedFilePath)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayerBR(Cube3x3(solvedState))

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.middleLayer3x3BRExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-middle-layer-br-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-middle-layer-br-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3MiddleLayerBR(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                              filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                              filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)"), AgentFile(filePathMidLayerBR, "middle layer (BR)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3MiddleLayerAll(midLayerBRSolvedFilePath: String, prevQValuesFilePath: Option[String]): Unit =
    val start = LocalDateTime.now()
    println(start)
    val epochEpisodes = 50000
    val solvedStates = Train.loadSimple(midLayerBRSolvedFilePath)

    val (agent, max, episodeMoves) = if(prevQValuesFilePath.isDefined)
      (Agent.load(prevQValuesFilePath.get, 0.10, 0.001, 2000L), 15000000, 30)
    else
      (Agent(0.20, 0.01, 7000L), 5000000, 30)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperLayerAll(Cube3x3(solvedState))


    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.middleLayer3x3AllExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-middle-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-middle-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3MiddleLayerAll(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                               filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                               filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                               filePathMidLayerAll: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)"), AgentFile(filePathMidLayerBR, "middle layer (BR)"),
      AgentFile(filePathMidLayerAll, "middle layer (all)")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3YellowCross(midLayerAllSolvedFilePath: String, prevQValuesFilePath: Option[String] = None): Unit =
    val start = LocalDateTime.now()
    println(start)
    val epochEpisodes = 50000
    val solvedStates = Train.loadSimple(midLayerAllSolvedFilePath)

    val (agent, max, episodeMoves) = if (prevQValuesFilePath.isDefined)
      (Agent.load(prevQValuesFilePath.get, 0.10, 0.001, 2000L), 1500000, 30)
    else
      (Agent(0.20, 0.01, 7000L), 5000000, 50)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      val c = Cube3x3.maskYellowCross(Cube3x3(solvedState))
      //if(c.maskedState == "...FFFFFF...LLLLLL...BBBBBB...RRRRRR...UU..U.DDDDDDDDD") println("!!!!!!!!!!!")
      //println(c.maskedState)
      c

      //...FFFFFF...LLLLLL...BBBBBB...RRRRRR...UUU...DDDDDDDDD
      //...FFFFFF...LLLLLL...BBBBBB...RRRRRR...UU..U.DDDDDDDDD


    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.yellowCross3x3ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-yellow-cross-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-yellow-cross-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def pretrainRL3x3YellowCross(solutionsFilePath: String): Unit =
    val solvedStates = Train.loadSimple(solutionsFilePath)
    val decoded = solvedStates.map(_.split('|')).map({case Array(state, solve, solvedState) => (state, solve, solvedState)})
    println(f"Pretraining using ${decoded.length} solved cubes from $solutionsFilePath")

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = decoded.foldLeft(agent)((a, d) =>
      val environment = Environment.init3x3(() => Cube3x3.maskYellowCross(Cube3x3(d._1)), Environment.yellowCross3x3ExpectedState)
      d._2.split(" ").foreach(a => environment.step(a))
      a.updateEpisode(environment)
    )
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-yellow-cross-pre-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-yellow-cross-pre-$timestampTxt.txt")


  def testRun3x3YellowCross(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                            filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                            filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                            filePathMidLayerAll: String, filePathYellowCross: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)"), AgentFile(filePathMidLayerBR, "middle layer (BR)"),
      AgentFile(filePathMidLayerAll, "middle layer (all)"), AgentFile(filePathYellowCross, "yellow cross")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def trainRL3x3YellowLayer(midLayerAllSolvedFilePath: String, prevQValuesFilePath: Option[String] = None): Unit =
    val start = LocalDateTime.now()
    println(start)
    val epochEpisodes = 50000
    val solvedStates = Train.loadSimple(midLayerAllSolvedFilePath)

    val (agent, max, episodeMoves) =
      if (prevQValuesFilePath.isDefined) (Agent.load(prevQValuesFilePath.get, 0.20, 0.001, 2000L), 1500000, 50)
      else (Agent(0.20, 0.01, 7000L), 5000000, 50)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskYellowLayer(Cube3x3(solvedState))

    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.yellowLayer3x3ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-yellow-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-yellow-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def testRun3x3YellowLayer(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                            filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                            filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                            filePathMidLayerAll: String, filePathYellowLayer: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)"), AgentFile(filePathMidLayerBR, "middle layer (BR)"),
      AgentFile(filePathMidLayerAll, "middle layer (all)"), AgentFile(filePathYellowLayer, "yellow layer")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def pretrainRL3x3YellowLayer(solutionsFilePath: String): Unit =
    val solvedStates = Train.loadSimple(solutionsFilePath)
    val decoded = solvedStates.map(_.split('|')).map({ case Array(state, solve, solvedState) => (state, solve, solvedState) })
    println(f"Pretraining using ${decoded.length} solved cubes from $solutionsFilePath")

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = decoded.foldLeft(agent)((a, d) =>
      val environment = Environment.init3x3(() => Cube3x3.maskYellowLayer(Cube3x3(d._1)), Environment.yellowLayer3x3ExpectedState)
      d._2.split(" ").foreach(a => environment.step(a))
      a.updateEpisode(environment)
    )
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-yellow-layer-pre-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-yellow-layer-pre-$timestampTxt.txt")


  def trainRL3x3UpperCorners(yellowLayerAllSolvedFilePath: String, prevQValuesFilePath: Option[String] = None): Unit =
    val start = LocalDateTime.now()
    println(start)
    val epochEpisodes = 50000
    val solvedStates = Train.loadSimple(yellowLayerAllSolvedFilePath)

    val (agent, max, episodeMoves) =
      if (prevQValuesFilePath.isDefined) (Agent.load(prevQValuesFilePath.get, 0.0001, 0.0001, 1000L), 1500000, 30) // least random
      else (Agent(0.20, 0.01, 7000L), 5000000, 50)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3.maskUpperEdges(Cube3x3(solvedState))

    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Environment.yellowCorners3x3ExpectedState), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-upper-corners-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-upper--$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def pretrainRL3x3UpperCorners(solutionsFilePath: String): Unit =
    val solvedStates = Train.loadSimple(solutionsFilePath)
    val decoded = solvedStates.map(_.split('|')).map({ case Array(state, solve, solvedState) => (state, solve, solvedState) })
    println(f"Pretraining using ${decoded.length} solved cubes from $solutionsFilePath")

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = decoded.foldLeft(agent)((a, d) =>
      val environment = Environment.init3x3(() => Cube3x3.maskUpperEdges(Cube3x3(d._1)), Environment.yellowCorners3x3ExpectedState)
      d._2.split(" ").foreach(a => environment.step(a))
      a.updateEpisode(environment)
    )
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-upper-corners-pre-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-upper-corners-pre-$timestampTxt.txt")


  def trainRL3x3UpperLayer(yellowLayerAllSolvedFilePath: String, prevQValuesFilePath: Option[String] = None): Unit =
    val start = LocalDateTime.now()
    println(start)
    val epochEpisodes = 50000
    val solvedStates = Train.loadSimple(yellowLayerAllSolvedFilePath)

    val (agent, max, episodeMoves) =
      if (prevQValuesFilePath.isDefined) (Agent.load(prevQValuesFilePath.get, 0.0, 0.0, 1000L), 1000000, 50) // least random
      else (Agent(0.20, 0.01, 7000L), 5000000, 50)

    def prepareCube(): Cube =
      val solvedState = solvedStates(scala.util.Random.nextInt(solvedStates.length))
      Cube3x3(solvedState)

    val afterAgent = Train.iteration(agent, () => Environment.init3x3(() => prepareCube(), Cube3x3.SOLVED_STATE), max, max, episodeMoves, epochEpisodes, 0)
    agent.printStats(max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-upper-layer-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-upper-layer-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")


  def pretrainRL3x3UpperLayer(solutionsFilePath: String): Unit =
    val solvedStates = Train.loadSimple(solutionsFilePath)
    val decoded = solvedStates.map(_.split('|')).map({ case Array(state, solve, solvedState) => (state, solve, solvedState) })
    println(f"Pretraining using ${decoded.length} solved cubes from $solutionsFilePath")

    val agent = Agent(0.20, 0.01, 7000L)
    val afterAgent = decoded.foldLeft(agent)((a, d) =>
      val environment = Environment.init3x3(() => Cube3x3(d._1), Cube3x3.SOLVED_STATE)
      d._2.split(" ").foreach(a => environment.step(a))
      a.updateEpisode(environment)
    )
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-upper-layer-pre-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-upper-layer-pre-$timestampTxt.txt")


  def testRun3x3UpperLayer(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                            filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                            filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                            filePathMidLayerAll: String, filePathYellowLayer: String, filePathUpperLayer: String): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)"), AgentFile(filePathMidLayerBR, "middle layer (BR)"),
      AgentFile(filePathMidLayerAll, "middle layer (all)"), AgentFile(filePathYellowLayer, "yellow layer"),
      AgentFile(filePathUpperLayer, "upper layer")))
    val results = Train.startSolving(stages, envGenerator, stageConfig, 100000, 10000, false)
    println(results.res.toVector.sortBy(_._1).mkString("\n"))


  def solveOneFromState(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                           filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                           filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                           filePathMidLayerAll: String, filePathYellowLayer: String, filePathUpperLayer: String,
                           initialState: String): Unit =
    val initialCube = Cube3x3(initialState)
    println(f"initial state: ${initialCube.state}")
    solveOne(filePathWhiteCross, filePathWhiteLayerFL, filePathWhiteLayerLB,
      filePathWhiteLayerBR, filePathWhiteLayerAll, filePathMidLayerFL, filePathMidLayerLB, filePathMidLayerBR,
      filePathMidLayerAll, filePathYellowLayer, filePathUpperLayer, initialCube)

  def solveOneFromScramble(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                           filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                           filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                           filePathMidLayerAll: String, filePathYellowLayer: String, filePathUpperLayer: String,
                           initialScramble: String): Unit =
    val startSolved = Cube3x3.solved
    println(startSolved.printableState)
    val initialCube = initialScramble.split(" ").map(Moves3x3.from).foldLeft(Cube3x3.maskAllExceptWhiteCross(Cube3x3.solved3x3))((c, m) =>
      println(f"scramble move: ${m.symbol}")
      val res = m.applyToCube(c)
      println(res.printableState)
      res
    )
    println(f"initial scramble: $initialScramble initial state: ${initialCube.state}")
    solveOne(filePathWhiteCross, filePathWhiteLayerFL, filePathWhiteLayerLB,
      filePathWhiteLayerBR, filePathWhiteLayerAll, filePathMidLayerFL, filePathMidLayerLB, filePathMidLayerBR,
      filePathMidLayerAll, filePathYellowLayer, filePathUpperLayer, initialCube)

  def solveRandomOne(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
                     filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
                     filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
                     filePathMidLayerAll: String, filePathYellowLayer: String,
                     filePathUpperLayer: String): Unit =
    val moves = Moves3x3.randomList(scala.util.Random.nextInt(5) + 10).map(_.symbol).mkString(" ")
    solveOneFromScramble(filePathWhiteCross, filePathWhiteLayerFL, filePathWhiteLayerLB,
      filePathWhiteLayerBR, filePathWhiteLayerAll, filePathMidLayerFL, filePathMidLayerLB,
      filePathMidLayerBR, filePathMidLayerAll, filePathYellowLayer, filePathUpperLayer, moves)

  def solveOne(filePathWhiteCross: String, filePathWhiteLayerFL: String, filePathWhiteLayerLB: String,
               filePathWhiteLayerBR: String, filePathWhiteLayerAll: String,
               filePathMidLayerFL: String, filePathMidLayerLB: String, filePathMidLayerBR: String,
               filePathMidLayerAll: String, filePathYellowLayer: String, filePathUpperLayer: String, initialCube: Cube): Unit =
    val stages = Train.loadAgents(Vector(AgentFile(filePathWhiteCross, "white cross"), AgentFile(filePathWhiteLayerFL, "white layer (FL)"),
      AgentFile(filePathWhiteLayerLB, "white layer (LB)"), AgentFile(filePathWhiteLayerBR, "white layer (BR)"),
      AgentFile(filePathWhiteLayerAll, "white layer (all)"), AgentFile(filePathMidLayerFL, "middle layer (FL)"),
      AgentFile(filePathMidLayerLB, "middle layer (LB)"), AgentFile(filePathMidLayerBR, "middle layer (BR)"),
      AgentFile(filePathMidLayerAll, "middle layer (all)"), AgentFile(filePathYellowLayer, "yellow layer"),
      AgentFile(filePathUpperLayer, "upper layer")))
    println(f"initial:\n${initialCube.printableState}\n")
    val results = Train.startSolvingSingle(initialCube.state, stages, envGeneratorSingle, stageConfig, true)
    val moves = results.res.toVector.map(_._1).head
    if (moves < 0) println("Unsolved") else println(f"Solved in $moves moves")


  // Random initial state
  val envGenerator: Map[String, String => Environment] = Map(
    "white cross" -> (_ => Environment.init3x3WhiteCrossTraining(10 + scala.util.Random.nextInt(20))),
    "white layer (FL)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersFL(Cube3x3(state)), Environment.whiteLayer3x3FLExpectedState)),
    "white layer (LB)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersLB(Cube3x3(state)), Environment.whiteLayer3x3LBExpectedState)),
    "white layer (BR)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersBR(Cube3x3(state)), Environment.whiteLayer3x3BRExpectedState)),
    "white layer (all)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayersAll(Cube3x3(state)), Environment.whiteLayer3x3AllExpectedState)),
    "middle layer (FL)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayerFL(Cube3x3(state)), Environment.middleLayer3x3FLExpectedState)),
    "middle layer (LB)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayerLB(Cube3x3(state)), Environment.middleLayer3x3LBExpectedState)),
    "middle layer (BR)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayerBR(Cube3x3(state)), Environment.middleLayer3x3BRExpectedState)),
    "middle layer (all)" -> (state => Environment.init3x3(() => Cube3x3.maskUpperLayerAll(Cube3x3(state)), Environment.middleLayer3x3AllExpectedState)),
    //"yellow cross" -> (state => Environment.init3x3(() => Cube3x3.maskYellowCross(Cube3x3(state)), Environment.yellowCross3x3ExpectedState)),
    "yellow layer" -> (state => Environment.init3x3(() => Cube3x3.maskYellowLayer(Cube3x3(state)), Environment.yellowLayer3x3ExpectedState)),
    "yellow corners" -> (state => Environment.init3x3(() => Cube3x3.maskUpperEdges(Cube3x3(state)), Environment.yellowCorners3x3ExpectedState)),
    "upper layer" -> (state => Environment.init3x3(() => Cube3x3(state), Cube3x3.SOLVED_STATE))
  )

  // Given initial state
  val envGeneratorStartState: Map[String, String => Environment] = Map(
    "white cross" -> (state => Environment.init3x3(() => Cube3x3.maskAllExceptWhiteCross(Cube3x3(state)), Environment.whiteCross3x3ExpectedState))
  )

  // Full stage config for given initial state
  val envGeneratorSingle: Map[String, String => Environment] = envGenerator ++ envGeneratorStartState


  // Parameters for testRunStage (moves, precision, epsilon)
  val stageConfig: Map[String, StageConfig] = Map(
    "white cross" -> StageConfig(100, 0.001, 0.0), // No randomness
    "white layer (FL)" -> StageConfig(100, 0.001, 0.0),
    "white layer (LB)" -> StageConfig(100, 0.001, 0.0),
    "white layer (BR)" -> StageConfig(100, 0.001, 0.0),
    "white layer (all)" -> StageConfig(100, 0.001, 0.0),
    "middle layer (FL)" -> StageConfig(100, 0.001, 0.0),
    "middle layer (LB)" -> StageConfig(100, 0.001, 0.0),
    "middle layer (BR)" -> StageConfig(100, 0.001, 0.0),
    "middle layer (all)" -> StageConfig(100, 0.001, 0.0),
    //"yellow cross" -> StageConfig(100, 0.001, 0.0),
    "yellow layer" -> StageConfig(100, 0.001, 0.0),
    "yellow corners" -> StageConfig(100, 0.001, 0.0),
    "upper layer" -> StageConfig(100, 0.001, 0.0),
  )