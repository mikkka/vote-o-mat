package name.mtkachev.voteomat.util

import cats.FlatMap

case class Lens[O, V](
    get: O => V,
    set: (O, V) => O
)

object Lens {
  def compose[Outer, Inner, Value](
      outer: Lens[Outer, Inner],
      inner: Lens[Inner, Value]
  ) = Lens[Outer, Value](
    get = outer.get andThen inner.get,
    set = (obj, value) => outer.set(obj, inner.set(outer.get(obj), value))
  )
}

case class LensT[M[_], O, V](
    get: O => M[V],
    set: (O, V) => M[O]
)

object LensT {
  import cats.syntax.flatMap._

  def compose[M[_], Outer, Inner, Value](
      outer: LensT[M, Outer, Inner],
      inner: LensT[M, Inner, Value]
  )(implicit ev: FlatMap[M]) =
    LensT[M, Outer, Value](
      get = outer.get andThen (x => x.flatMap(inner.get)),
      set = (obj, value) => {
        outer.get(obj).flatMap { i0 =>
          inner.set(i0, value).flatMap { i1 =>
            outer.set(obj, i1)
          }
        }
      }
    )
}
