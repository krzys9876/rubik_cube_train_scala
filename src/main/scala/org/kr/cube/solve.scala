package org.kr.cube

import org.kr.cube.rl.*

@main
def solve(args: String*): Unit =
  val argsV = args.toVector
  println(f"arguments: ${argsV.mkString("|")}")
  argsV match
    case Vector(initialState3x3)  if initialState3x3.length >= 54 =>
      Train3x3.solveOneFromState(
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
        "training/q-values/q-values-3x3-upper-layer-1000000-20260207_181739.txt",
        initialState3x3
      )
    case Vector(initialState2x2)  if initialState2x2.length < 54 =>
      Train2x2.solveOneFromState(
        "training/q-values/q-values-2x2-white-layer-2000000-20260205_110931.txt",
        "training/q-values/q-values-2x2-yellow-layer-2000000-20260205_111250.txt",
        "training/q-values/q-values-2x2-upper-layer-10000000-20260205_112548.txt",
        initialState2x2
        )
    case _ =>

