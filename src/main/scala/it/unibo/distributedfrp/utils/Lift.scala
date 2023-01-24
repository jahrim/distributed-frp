package it.unibo.distributedfrp.utils

trait Lift[F[_]]:
  def lift[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C]

object Lift:
  def lift[A, B, C, F[_] : Lift](a: F[A], b: F[B])(f: (A, B) => C): F[C] =
    summon[Lift[F]].lift(a, b)(f)

  def lift2[A, B, C, F1[_] : Lift, F2[_] : Lift](a: F1[F2[A]], b: F1[F2[B]])(f: (A, B) => C): F1[F2[C]] =
    lift(a, b)((aa, bb) => lift(aa, bb)(f))

  def lift3[A, B, C, F1[_] : Lift, F2[_] : Lift, F3[_] : Lift](a: F1[F2[F3[A]]], b: F1[F2[F3[B]]])(f: (A, B) => C): F1[F2[F3[C]]] =
    lift2(a, b)((aa, bb) => lift(aa, bb)(f))