package x.ai

object V1 {

  sealed trait Trampoline[+A] {
    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
      More[B](() => f(runT))

    final def   runT: A =
      this match {
        case More(k) => k().runT
        case Done(v) => v
      }
  }

  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

  case class Done[+A](result: A) extends Trampoline[A]

}

object V2 {

  sealed trait Trampoline[+A] {

    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
      this match {
        case FlatMap(a, g) =>
          FlatMap(a, (x: Any) => g(x) flatMap f)
        case x             =>
          FlatMap(x, f)
      }

    final def resume: Either[() => Trampoline[A], A] =
      this match {
        case Done(v) => Right(v)
        case More(k) => Left(k)
        case FlatMap(a, f) => a match {

          case Done(v) => f(v).resume

          case More(k) => Left(() => FlatMap(k(), f))

          /**
            * FlatMap( FlatMap(b, g), f )
            *
            * It’s critical to resolve this case in such a way that remains
            * productive without introducing new stack frames. The trick
            * is to re-associate the expression to the right:
            *
            * FlatMap( FlatMap(a, g), f ) -> FlatMap( a, x => FlatMap( g( x ), f ) )
            */
          case FlatMap(b, g) => b.flatMap((x : Any) => g(x).flatMap(f)).resume
            // (FlatMap(b, (x: Any) => FlatMap(g(x), f)): Trampoline[A]).resume

          /**
            * Also note that when we look inside the nested FlatMap
            * constructors, there is some type information that has been
            * lost. In a pattern like FlatMap(FlatMap(b, g), f) the
            * type of b cannot be known, so we must assume Any when
            * we construct the right-associated nesting. This is perfectly
            * safe, since we can assume the left-associated nesting was
            * well typed when it was constructed.
            *
            * FlatMap(
            *   FlatMap( a: F[A], g: A => F[B] ): F[B],
            *   f: B => F[C]
            * ): F[C]
            *
            * FlatMap(
            *   a: F[A],
            *   x: A => FlatMap( g( x ): F[B], f: B => F[C] ): F[C]
            * ): F[C]
            *
            * This re-association is taking advantage of the monad
            * laws. Trampoline is a monad, and monads are by definition
            * associative. Therefore the right-associated continuations are
            * always exactly equivalent to the left-associated ones.
            */
        }
      }

    final def runT: A = resume match {
      case Right(a) => a
      case Left(k) => k().runT
    }

    def zip[B](tb: Trampoline[B]): Trampoline[(A, B)] =
      (resume, tb.resume) match {
        case (Right(a), Right(b)) => Done((a, b))
        case (Left(a) , Left(b) ) => More(() => a() zip b())
        case (Left(a) , Right(b)) => More(() => a() zip Done(b))
        case (Right(a), Left(b) ) => More(() => Done(a) zip b())
      }
  }

  case class More[+A](
    k: () => Trampoline[A]
  ) extends Trampoline[A]

  case class Done[+A](result: A)
    extends Trampoline[A]

  case class FlatMap[A, +B](
    sub: Trampoline[A],
    k: A => Trampoline[B]
  ) extends Trampoline[B]


}

object TrampolineAsFree {
  sealed trait Free[S[+_], +A]

  case class More[S[+_], +A](
    k: S[Free[S, A]]
  ) extends Free[S, A]

  case class Done[S[+_], +A](
    result: A
  ) extends Free[S, A]

  case class FlatMap[S[+_], A, +B](
    sub: Free[S, A],
    k: A => Free[S, B]
  ) extends Free[S, B]

  // Where Free == Trampoline where `type S[T] = () => T`
}
