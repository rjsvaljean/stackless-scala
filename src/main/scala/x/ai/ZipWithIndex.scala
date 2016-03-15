package x.ai

import scala.annotation.tailrec

object ZipWithIndex {
  def zipIndex[A](as: List[A]): List[(A, Int)] = {
    val toIndexedList: SimpleState[Int, List[(Int, A)]] =
      as.foldLeft(
        SimpleState.pure[Int, List[(Int, A)]](List())
      ) { (state, currentElement) =>
        for {
          soFarIndexedList <- state
          currentIndex <- SimpleState.get
          _ <- SimpleState.set(currentIndex + 1)
        } yield
          (currentIndex, currentElement) ::
          soFarIndexedList
      }

    toIndexedList.runS(0)._1.reverse.map(_.swap)
  }

  def zipIndexV2[A](as: List[A]): List[(A, Int)] = {
    val toIndexedList: State[Int, List[(Int, A)]] =
      as.foldLeft(
        State.pure[Int, List[(Int, A)]](List())
      ) { (state, currentElement) =>
        for {
          soFarIndexedList <- state
          currentIndex <- State.get
          _ <- State.set(currentIndex + 1)
        } yield
          (currentIndex, currentElement) ::
          soFarIndexedList
      }

    toIndexedList.runS(0).runT._1.reverse.map(_.swap)
  }

  def zipIndexTailCallEliminated[A](as: List[A]): List[(A, Int)] =
    foldLeft((0, List[(A, Int)]()), as) { case ((prevIndex, outList), a) =>
      (prevIndex + 1, outList :+ (a, prevIndex))
    }._2


  @tailrec
  def foldLeft[B, A](empty: B, as: List[A])(f: (B, A) => B): B = {
    as match {
      case Nil => empty
      case a :: _as => foldLeft(f(empty, a), _as)(f)
    }
  }


}
