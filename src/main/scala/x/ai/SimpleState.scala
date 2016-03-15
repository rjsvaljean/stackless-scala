package x.ai

case class SimpleState[S, +A](runS: S => (A, S)) {
  def map[B](f: A => B) =
    SimpleState[S, B](s => {
      val (a, s1) = runS(s)
      (f(a), s1)
    })

  def flatMap[B](f: A => SimpleState[S, B]) =
    SimpleState[S, B](s => {
      val (a, s1) = runS(s)
      f(a) runS s1
    })
}

object SimpleState {

  def get[S]: SimpleState[S, S] =
    SimpleState(s => (s, s))

  def set[S](s: S): SimpleState[S, Unit] =
    SimpleState(_ => ((), s))

  def pure[S, A](a: A): SimpleState[S, A] =
    SimpleState(s => (a, s))
}

case class StateUsingTrampolineV1[S, A](runS: S => V1.Trampoline[(A, S)]) {
  def flatMap1[B](f: A => StateUsingTrampolineV1[S, B]) =
    StateUsingTrampolineV1[S, B] {
      (s: S) => V1.More(() => {
        val (a, s1) = runS(s).runT
        V1.More(() => f(a) runS s1)
      })
    }

  def flatMap[B](f: A => StateUsingTrampolineV1[S, B]) =
    StateUsingTrampolineV1[S, B] {
      (s: S) => V1.More {
        () => runS(s).flatMap {
          case (a, s1) => V1.More(() => f(a) runS s1)
        }
      }
    }
}

case class State[S, A](runS: S => V2.Trampoline[(A, S)]) {

  def map[B](f: A => B) = flatMap(f andThen State.pure[S, B])

  def flatMap[B](f: A => State[S, B]) =
    State[S, B] {
      (s: S) => V2.More {
        () => runS(s).flatMap {
          case (a, s1) => V2.More(() => f(a) runS s1)
        }
      }
    }
}


object State {
  def get[S]: State[S, S] =
    State(s => V2.Done((s, s)))

  def set[S](s: S): State[S, Unit] =
    State(_ => V2.Done(((), s)))

  def pure[S, A](a: A): State[S, A] =
    State(s => V2.Done((a, s)))
}