package org.kr.cube.rl

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object Train3x3:
  def trainRL3x3WhiteCross(): Unit =
    val start = LocalDateTime.now()
    println(start)
    val max = 2000000
    val epochEpisodes = 50000
    val episodeMoves = 50
    val agent = Agent(0.25, 0.05, 5000L) // The most random and exploratory
    val afterAgent = Train.iteration(agent, () => Environment.init3x3WhiteCrossTraining(1 + scala.util.Random.nextInt(50)), max, max, episodeMoves, epochEpisodes, 0)
    Train.printAgentStats(afterAgent, max)
    val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
    println("Saving q-values to file")
    afterAgent.saveQState(f"q-values-3x3-white-cross-$max-$timestampTxt.txt")
    afterAgent.saveSolvedStates(f"solved-3x3-white-cross-$max-$timestampTxt.txt")
    val end = LocalDateTime.now()
    println(end)
    val diffSec = start.until(end, ChronoUnit.SECONDS)
    println(f"Time: $diffSec seconds / ${diffSec / 3600}:${(diffSec % 3600) / 60}%02d:${diffSec % 60}%02d")

