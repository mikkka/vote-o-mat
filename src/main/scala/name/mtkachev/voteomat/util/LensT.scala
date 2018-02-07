package name.mtkachev.voteomat.util

import cats.Monad

import scala.language.higherKinds

trait LensGetT[M[_], O, V] {
  def get: O => M[V]
}

trait LensSetT[M[_], O, V] {
  def set: (O, V) => M[O]
}

case class LensT[M[_], O, V] (
    override val get: O => M[V],
    override val set: (O, V) => M[O]
) extends LensGetT[M, O, V] with LensSetT[M, O, V]

object LensT {
  import cats.syntax.flatMap._

  def compose[M[_], Outer, Inner, Value](
      outer: LensT[M, Outer, Inner],
      inner: LensT[M, Inner, Value]
  )(implicit ev: Monad[M]) =
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



  def lens[M[_], O, V](_get: O => V, _set: (O, V) => O)(implicit ev: Monad[M]) = LensT(
    get = _get andThen ev.pure,
    set = (o: O, v: V) => ev.pure(_set(o, v))
  )

  def lens0[M[_], O, V](_get: O => V, _set: (O, V) => M[O])(implicit ev: Monad[M]) = LensT(
    get = _get andThen ev.pure,
    set = _set
  )

  def lens1[M[_], O, V](_get: O => M[V], _set: (O, V) => O)(implicit ev: Monad[M]) = LensT(
    get = _get,
    set = (o: O, v: V) => ev.pure(_set(o, v))
  )

  def lensT[M[_], O, V](_get: O => M[V], _set: (O, V) => M[O])(implicit ev: Monad[M]) = LensT(
    get = _get,
    set = _set
  )
}
