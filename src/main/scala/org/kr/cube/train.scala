package org.kr.cube

import org.kr.cube.rl.{Agent, Environment, EpochLog, Train2x2, Train3x3}

@main
def main(): Unit =

  //Train3x3.trainRL3x3WhiteCross()
  //Train3x3.testRun3x3WhiteCross("q-values-3x3-white-cross-1000000-20260205_142414.txt")
  //Train3x3.trainRL3x3WhiteLayerFL("solved-3x3-white-cross-1000000-20260205_142414.txt")
  //Train3x3.testRun3x3WhiteLayerFL("q-values-3x3-white-cross-1000000-20260205_142414.txt", "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt")
  //Train3x3.trainRL3x3WhiteLayerLB("solved-3x3-white-layer-fl-1500000-20260205_225856.txt")
  /*Train3x3.testRun3x3WhiteLayerLB(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt"
  )*/
  //Train3x3.trainRL3x3WhiteLayerBR("solved-3x3-white-layer-lb-1500000-20260205_230535.txt")
  /*Train3x3.testRun3x3WhiteLayerBR(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt"
  )*/
  //Train3x3.trainRL3x3WhiteLayerAll("solved-3x3-white-layer-br-1500000-20260205_231443.txt")
  /*Train3x3.testRun3x3WhiteLayerAll(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt"
  )*/

  /*Train3x3.solveRandomOne(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
    "q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt",
    "q-values-3x3-middle-layer-br-5000000-20260206_081021.txt",
    "q-values-3x3-middle-layer-1500000-20260206_092530.txt",
    "q-values-3x3-yellow-layer-1500000-20260206_162024.txt",
    "q-values-3x3-upper-layer-1000000-20260207_181739.txt"
  )*/
/*Train3x3.solveOneFromState(
  "q-values-3x3-white-cross-1000000-20260205_142414.txt",
  "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
  "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
  "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
  "q-values-3x3-white-layer-1500000-20260205_232052.txt",
  "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
  "q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt",
  "q-values-3x3-middle-layer-br-5000000-20260206_081021.txt",
  "q-values-3x3-middle-layer-1500000-20260206_092530.txt",
  "q-values-3x3-yellow-layer-1500000-20260206_162024.txt",
  "q-values-3x3-upper-layer-1000000-20260207_181739.txt",
  "rrbdfubld dffflrddd fdlbblbur ullfrrrrl fbuduuuur lbbldffbu"
)*/
//Train3x3.trainRL3x3MiddleLayerFL("solved-3x3-white-layer-1500000-20260205_232052.txt")

  /*Train3x3.testRun3x3MiddleLayerFL(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt"
  )*/
  //Train3x3.trainRL3x3MiddleLayerLB("solved-3x3-middle-layer-fl-5000000-20260206_010357.txt")

  /*Train3x3.testRun3x3MiddleLayerLB(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
    "q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt"
  )*/
  //Train3x3.trainRL3x3MiddleLayerBR("solved-3x3-middle-layer-lb-5000000-20260206_012056.txt")

  /*Train3x3.testRun3x3MiddleLayerBR(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
    "q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt",
    "q-values-3x3-middle-layer-br-5000000-20260206_081021.txt"
  )*/

  //Train3x3.trainRL3x3MiddleLayerAll("solved-3x3-middle-layer-br-5000000-20260206_081021.txt")
  //Train3x3.trainRL3x3MiddleLayerAll("solved-3x3-middle-layer-br-5000000-20260206_081021.txt", Some("q-values-3x3-middle-layer-5000000-20260206_085253.txt"))

  /*Train3x3.testRun3x3MiddleLayerAll(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
    "q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt",
    "q-values-3x3-middle-layer-br-5000000-20260206_081021.txt",
    "q-values-3x3-middle-layer-1500000-20260206_092530.txt"
  )*/

  //Train3x3.pretrainRL3x3YellowLayer("yellowLayerInput.txt")
  //Train3x3.trainRL3x3YellowLayer("solved-3x3-middle-layer-1500000-20260206_092530.txt", Some("q-values-3x3-yellow-layer-pre-20260206_160946.txt"))

  /*Train3x3.testRun3x3YellowLayer(
    "q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
    "q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt",
    "q-values-3x3-middle-layer-br-5000000-20260206_081021.txt",
    "q-values-3x3-middle-layer-1500000-20260206_092530.txt",
    "q-values-3x3-yellow-layer-1500000-20260206_162024.txt"
  )*/
  //Train3x3.pretrainRL3x3UpperCorners("yellowCornersInput.txt")
  //Train3x3.trainRL3x3UpperCorners("solved-3x3-yellow-layer-1500000-20260206_162024.txt", Some("q-values-3x3-upper-corners-pre-20260207_175010.txt"))
  //Train3x3.pretrainRL3x3UpperLayer("upperLayerInput.txt")
  //Train3x3.trainRL3x3UpperLayer("solved-3x3-yellow-layer-1500000-20260206_162024.txt", Some("q-values-3x3-upper-layer-pre-20260207_125542.txt"))
  Train3x3.testRun3x3UpperLayer(
    "training/q-values/q-values-3x3-white-cross-1000000-20260205_142414.txt",
    "training/q-values/q-values-3x3-white-layer-fl-1500000-20260205_225856.txt",
    "training/q-values/q-values-3x3-white-layer-lb-1500000-20260205_230535.txt",
    "training/q-values/q-values-3x3-white-layer-br-1500000-20260205_231443.txt",
    "training/q-values/q-values-3x3-white-layer-1500000-20260205_232052.txt",
    "training/q-values/q-values-3x3-middle-layer-fl-5000000-20260206_010357.txt",
    "training/q-values/q-values-3x3-middle-layer-lb-5000000-20260206_012056.txt",
    "training/q-values/q-values-3x3-middle-layer-br-5000000-20260206_081021.txt",
    "training/q-values/q-values-3x3-middle-layer-1500000-20260206_092530.txt",
    "training/q-values/q-values-3x3-yellow-layer-1500000-20260206_162024.txt",
    "training/q-values/q-values-3x3-upper-layer-1000000-20260207_181739.txt"
  )


///////////////////////////////////////////////////////////////////////////////////
  //Train2x2.trainRL2x2WhiteLayer()
  //Train2x2.testRun2x2WhiteLayer("q-values-2x2-white-layer-2000000-20260205_110931.txt")
  //Train2x2.trainRL2x2YellowLayer("solved-2x2-white-layer-2000000-20260205_110931.txt")
  //Train2x2.testRun2x2YellowLayer("q-values-2x2-white-layer-2000000-20260205_110931.txt", "q-values-2x2-yellow-layer-2000000-20260205_111250.txt")
  //Train2x2.trainRL2x2UpperLayer("solved-2x2-yellow-layer-2000000-20260205_111250.txt")
  /*Train2x2.testRun2x2UpperLayer(
    "q-values-2x2-white-layer-2000000-20260205_110931.txt",
    "q-values-2x2-yellow-layer-2000000-20260205_111250.txt",
    "q-values-2x2-upper-layer-10000000-20260205_112548.txt"
  )*/
  //debugUpperLayer("q-values-2x2-upper-layer-10000000-20260203_094831.txt")
  /*Train2x2.solveOneFromScramble(
    "q-values-2x2-white-layer-2000000-20260205_110931.txt",
    "q-values-2x2-yellow-layer-2000000-20260205_111250.txt",
    "q-values-2x2-upper-layer-10000000-20260205_112548.txt",
    "B B L' R B R B' U F' R U D R' B' D"
  )*/

  /*Train2x2.solveOneFromState(
    "q-values-2x2-white-layer-2000000-20260205_110931.txt",
    "q-values-2x2-yellow-layer-2000000-20260205_111250.txt",
    "q-values-2x2-upper-layer-10000000-20260205_112548.txt",
    //"BFULLRFBFUBDLUDRFRUDLBRD"
    //"UDFLULDUDBRBLFFURRBBLDRF"
    //"UFRBLRRFDDBDULRLBFFLDUBU"
    "LDRFFBBUDLLUBBRDULURFDRF"
  )*/

  /*(0 until 1).foreach(_ =>
  Train2x2.solveRandomOne(
    "q-values-2x2-white-layer-2000000-20260205_110931.txt",
    "q-values-2x2-yellow-layer-2000000-20260205_111250.txt",
    "q-values-2x2-upper-layer-10000000-20260205_112548.txt"
  ))*/

  /*val cube = Cube3x3.solved
  println(cube.printableState)*/



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


