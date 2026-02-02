package org.kr.cube

import java.time.{Instant, LocalDateTime, Period}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec

@main
def main(): Unit = {
  val start = LocalDateTime.now()
  println(start)
  //whiteLayer()
  //rl()
  trainRL()
  val end = LocalDateTime.now()
  println(end)
  val diffSec = start.until(end, ChronoUnit.SECONDS)
  println(s"Time: $diffSec seconds")
}

def whiteLayer(): Unit =
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

// 1000000 - 25 min, need profiler to optimize
def rl(): Unit =
  val max = 1000000
  val agent = Agent(Map())
  val res = (0 until max).foldLeft((Vector[Environment](), agent))({case ((r, a), i) =>
    if(i % 10000 == 0) printAgentStats(a, max)
    val env = rlEpisode()
    if(env.isSolved)
      println(f"$i ${env.state} ${env.history.length} ${env.history.mkString(" ")}")
      (r.appended(env), a.updateEpisode(env))
    else (r, a)
  })
  println(f"solved in ${res._1.length} / $max attempts (${res._1.length.toDouble / max * 100.0}%.2f%%)")
  val (resEnv, resAgent) = res
  printAgentStats(resAgent, max)
  resAgent.saveQState("q-values.txt")

def rlEpisode(): Environment =
  val env = Environment.init(20, "..FF..LL..BB..RR....DDDD",
    (f: Face, t: Tile) => f.nominalFace == Face2x2.U || (f.axisV.symbol == "Y" && t.coords.r == 0))
  (0 until 200).foldLeft(env)((e, i) =>
    val action = Moves2x2.randomExceptOpposite(e.history.lastOption.map(_.action)).symbol
    if(!e.isSolved) e.step(action) else e)

def printAgentStats(agent: Agent, max: Long): Unit =
  println(LocalDateTime.now())
  val agg = agent.qState.groupBy({ case (_, (c, _)) => c }).map((c, entries) => c -> entries.size).toVector.sortBy(_._1).reverse
  val singleVisited = agg.filter(_._1 == 1).map(_._2).sum
  val allOther = agg.filter(_._1 > 1).map(_._2).sum
  //println(f"q-values stats (number of visits - number of states): \n${agg.mkString("\n")}")
  println(f"q-values: ${agent.qState.keys.size} keys, stats (number of visits - number of states): single visits: $singleVisited, other: $allOther, ratio ${singleVisited.toDouble / (singleVisited+allOther).toDouble * 100.0}%.3f%%")
  println(f"episodes: ${agent.episodeCount}, episodes ratio of $max: ${agent.episodeCount.toDouble / max * 100.0}%.3f%%, epsilon: ${agent.epsilon}")

def episode(agent: Agent, env: Environment): Environment =
  (0 until 200).foldLeft(env)((e, i) =>
    val action = agent.nextBestAction(env)
    if (!e.isSolved) e.step(action) else e
  )

@tailrec
def iteration(agent: Agent, toGo: Long, initialMax: Long): Agent =
  if (toGo == 0) agent
  else
    val initEnv = Environment.init(20, "..FF..LL..BB..RR....DDDD",
      (f: Face, t: Tile) => f.nominalFace == Face2x2.U || (f.axisV.symbol == "Y" && t.coords.r == 0))
    val afterEnvironment = episode(agent, initEnv)
    //if(afterEnvironment.isSolved) println(f"$toGo ${afterEnvironment.state} ${afterEnvironment.history.length}")
    if(toGo % 10000 == 0)  printAgentStats(agent, initialMax - toGo)
    iteration(agent.updateEpisode(afterEnvironment), toGo - 1, initialMax)

def trainRL(): Unit =
  val max = 1000000
  val agent = Agent()
  val afterAgent = iteration(agent, max, max)
  printAgentStats(afterAgent, max)
  val timestampTxt = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss").format(LocalDateTime.now())
  afterAgent.saveQState(f"q-val, maxues-trained-$max-$timestampTxt.txt")

