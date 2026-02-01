package org.kr.cube

import scala.annotation.tailrec

@main
def main(): Unit =
  //whiteLayer()
  rl()

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

def rl(): Unit =
  val max = 100000
  val agent = Agent(Map())
  val res = (0 until max).foldLeft((Vector[Environment](), agent))({case ((r, a), i) =>
    val env = rlEpisode()
    if(env.isSolved)
      println(f"$i ${env.state} ${env.history.length} ${env.history.mkString(" ")}")
      (r.appended(env), a.updateEpisode(env))
    else (r, a)
  })
  println(f"solved in ${res._1.length} / $max attempts (${res._1.length.toDouble / max * 100.0}%.2f%%)")
  println(f"q-values: ${res._2.qState.keys.size} keys")
  val agg = res._2.qState.groupBy(_._2.size).map(v => v._1 -> v._2.size).toVector.sortBy(_._1).reverse
  println(f"q-values stats: \n${agg.mkString("\n")}")
  res._2.saveQState("q-values.txt")

def rlEpisode(): Environment =
  val env = Environment.init(20, "..FF..LL..BB..RR....DDDD",
    (f: Face, t: Tile) => f.nominalFace == Face2x2.U || (f.axisV.symbol == "Y" && t.coords.r == 0))
  (0 until 200).foldLeft(env)((e, i) =>
    val action = Moves2x2.randomExceptOpposite(e.history.lastOption.map(_.action)).symbol
    if(!e.isSolved) e.step(action) else e)

