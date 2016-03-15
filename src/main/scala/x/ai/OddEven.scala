package x.ai

object OddEven {

  def even[A](ns: List[A]): Boolean =
    ns match {
      case Nil => true
      case x :: xs => odd(xs)
    }

  def odd[A](ns: List[A]): Boolean =
    ns match {
      case Nil => false
      case x :: xs => even(xs)
    }

  // Using TrampolineV1
  import V1._

  def evenTrampolined[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(true)
      case x :: xs => More(() => oddTrampolined(xs))
    }

  def oddTrampolined[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(false)
      case x :: xs => More(() => evenTrampolined(xs))
    }

//  evenTrampolined(List(1, 2, 3, 4)) == More(() => oddTrampolined(List(2, 3, 4)))
//  evenTrampolined(List(1, 2, 3, 4)) == More(() => More(() => evenTrampolined(List(3, 4))))
//  evenTrampolined(List(1, 2, 3, 4)) == More(() => More(() => More(() => oddTrampolined(List(4)))))
//  evenTrampolined(List(1, 2, 3, 4)) == More(() => More(() => More(() => More(() => evenTrampolined(List())))))
//  evenTrampolined(List(1, 2, 3, 4)) == More(() => More(() => More(() => More(() => Done(true)))))
//
//  More(() => More(() => More(() => More(() => Done(true)))))
//  More(() => More(() => More(() => Done(true))))
//  More(() => More(() => Done(true)))
//  More(() => Done(true))
//  Done(true)
//  true
}
