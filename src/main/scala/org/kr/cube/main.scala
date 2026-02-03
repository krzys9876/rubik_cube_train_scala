package org.kr.cube

import org.kr.cube.rl.{Agent, Environment, EpochLog, Train2x2}

import java.io.PrintWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.mutable

@main
def main(): Unit =
  //trainRL2x2WhiteLayer()
  //testRun2x2WhiteLayer("q-values-2x2-white-layer-3000000-20260203_110743.txt")
  //trainRL2x2YellowLayer("solved-2x2-white-layer-1000000-20260203_005055.txt")
  //testRun2x2YellowLayer("q-values-2x2-white-layer-3000000-20260203_141521.txt", "q-values-2x2-yellow-layer-1000000-20260203_143242.txt")
  //trainRL2x2UpperLayer("solved-2x2-yellow-layer-1000000-20260203_010048.txt")
  /*testRun2x2UpperLayer(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt"
  )*/
  //debugUpperLayer("q-values-2x2-upper-layer-10000000-20260203_094831.txt")
  /*solveOneFromScramble(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt",
    "B B L' R B R B' U F' R U D R' B' D"
  )*/

  Train2x2.solveOneFromState(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt",
    //"BFULLRFBFUBDLUDRFRUDLBRD"
    //"UDFLULDUDBRBLFFURRBBLDRF"
    //"UFRBLRRFDDBDULRLBFFLDUBU"
    "LDRFFBBUDLLUBBRDULURFDRF"
  )

  (0 until 100).foreach(_ =>
  Train2x2.solveRandomOne(
    "q-values-2x2-white-layer-3000000-20260203_141521.txt",
    "q-values-2x2-yellow-layer-1000000-20260203_143242.txt",
    "q-values-2x2-upper-layer-10000000-20260203_151354.txt"
  ))



/*def whiteLayer(): Unit =
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

*/


