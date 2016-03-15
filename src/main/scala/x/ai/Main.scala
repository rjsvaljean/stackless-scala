package x.ai

import ZipWithIndex._
import OddEven._

object Main extends App {
  private val tests: List[((Boolean, String), () => Any, Any)] = List(
    (
      (true, "zip with index using state monad"),
      () => zipIndex(List(1, 2, 3)),
      List(1, 2, 3).zipWithIndex
      ),
    (
      (true, "zip with index using state monad overflows stack"),
      () => zipIndex(List.fill(10000)(1)),
      List.fill(10000)(1).zipWithIndex
      ),
    (
      (true, "we can write avoid the stackoverflow using scala @tailrec"),
      () => zipIndexTailCallEliminated(List.fill(10000)(1)),
      List.fill(10000)(1).zipWithIndex
      ),
    (
      (true, "or using trampolining"),
      () => zipIndexV2(List.fill(10)(1)),
      List.fill(10)(1).zipWithIndex
      ),
    (
      (true, "but it isn't universally applicably. Only to self recursive calls" +
        ". Mutual recursion example with odd/even"),
      () => (even(List(1, 2)), odd(List(1))),
      (true, true)
      ),
    (
      (true, "as expected it stack overflows"),
      () => even(List.fill(1000000)(1)),
      true
      ),
    (
      (true, "moving from the stack to the heap. represent continuations as functions on the heap" +
        "Almost there but no cookie. Check out StateUsingTrampolineV1"),
      () => evenTrampolined(List.fill(1000000)(1)).runT,
      true
      )
  )


  tests.foreach {
    case ((true, label), test, expected) =>
      try {
        val actual = test()
        val passed = actual == expected
        if (passed) println(s"+ $label: $passed")
        else println(s"- $label: $actual =/= $expected")
      } catch {
        case t: Throwable => println(s"--- $label: Threw a $t")
      }
    case _ => ()
  }
}
