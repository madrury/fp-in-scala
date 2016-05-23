package fpinscala

case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = State(
        s => {
            val (a, s1) = run(s)
            (f(a), s1)
        }
    )

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(
        s => {
            val (a, s1) = run(s)
            f(a).run(s1)
        }
    )

    def map2[B, C](bs: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(aa => bs map(bb => f(aa, bb)))

}

object State {

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
        rs.foldRight(State.unit[S, List[A]](List[A]()))(_.map2(_)(_ :: _))

}
