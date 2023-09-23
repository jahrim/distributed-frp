package it.unibo.distributedfrp.frp

import it.unibo.distributedfrp.utils.Liftable
import nz.sodium.{Cell, CellLoop, Lambda0, Lambda1, Lambda2, Lambda3, Lambda4, Lambda5, Lambda6, Handler, Lazy, Operational, Stream, StreamLoop, Transaction, Tuple2 as BiTuple}
import java.util.Optional
import nz.sodium.time.SecondsTimerSystem

object FrpExtensions:
  given consumerToHandler[A]: Conversion[(A) => Unit, Handler[A]] = identity
  given function0ToLambda0[R]: Conversion[() => R, Lambda0[R]] = identity
  given function1ToLambda1[A,R]: Conversion[(A) => R, Lambda1[A,R]] = identity
  given function2ToLambda2[A,B,R]: Conversion[(A,B) => R, Lambda2[A,B,R]] = identity
  given function3ToLambda3[A,B,C,R]: Conversion[(A,B,C) => R, Lambda3[A,B,C,R]] = identity
  given function4ToLambda4[A,B,C,D,R]: Conversion[(A,B,C,D) => R, Lambda4[A,B,C,D,R]] = identity
  given function5ToLambda5[A,B,C,D,E,R]: Conversion[(A,B,C,D,E) => R, Lambda5[A,B,C,D,E,R]] = identity
  given function6ToLambda6[A,B,C,D,E,F,R]: Conversion[(A,B,C,D,E,F) => R, Lambda6[A,B,C,D,E,F,R]] = identity

  given Liftable[Cell] with
    def lift[A, B](a: Cell[A])(f: A => B): Cell[B] =
      val fLambda: Lambda1[A, B] = f(_)
      a.map(fLambda)

    override def lift[A, B, C](a: Cell[A], b: Cell[B])(f: (A, B) => C): Cell[C] =
      a.lift(b, (aa, bb) => f(aa, bb))

    override def lift[A, B, C, D](a: Cell[A], b: Cell[B], c: Cell[C])(f: (A, B, C) => D): Cell[D] =
      a.lift(b, c, (aa, bb, cc) => f(aa, bb, cc))

  extension[A] (stream: Stream[Option[A]])
    def onlyIfDefined: Stream[A] = stream.filter(_.isDefined).map(_.get)

  extension[A] (stream: Stream[A])
    def filterMap[B](f: A => Option[B]): Stream[B] = stream.map(f(_)).onlyIfDefined

    def filterByPrevious(predicate: (A, A) => Boolean, init: Lazy[Option[A]] = new Lazy(None)): Stream[A] =
      stream.collectLazy[Option[A], Option[A]](init, (next, prev) => {
        val someNext: Option[A] = Some(next)
        if prev.forall(p => predicate(p, next)) then new BiTuple(someNext, someNext) else new BiTuple(None, prev)
      }).onlyIfDefined
    
    def bufferByTime[T](timerSystem: SecondsTimerSystem, span: Double, f: Iterable[A] => T): Stream[T] =
      enum BufferInput:
        case Flush
        case Event(value: A, time: Double)

      enum BufferOutput:
        case Emit(value: T)
        case Stored
        case Reset(time: Double)

      import BufferInput._
      import BufferOutput._
      Transaction.run(() => {
        val flushRequests = new StreamLoop[Unit]
        val eventEmissions: Stream[BufferInput] = stream
          .snapshot(timerSystem.time.map(_.doubleValue), (a, t) => Event(a, t))
        val bufferEvents: Stream[BufferInput] = eventEmissions.orElse(flushRequests.map(_ => Flush))
        val out = bufferEvents
          .collect[BufferOutput, List[A]](List.empty, (event, state) =>
            (event, state) match
              case (Flush, buffer) => new BiTuple(Emit(f(buffer.reverse)), List.empty)
              case (Event(e, _), h :: t) => new BiTuple(Stored, e :: h :: t)
              case (Event(e, t), Nil) => new BiTuple(Reset(t), e :: Nil)
          )
        flushRequests.loop {
          val alarm = out.filterMap[Optional[java.lang.Double]] {
            case Reset(t) => Some(Optional.of(t + span))
            case Emit(_) => Some(Optional.empty)
            case _ => None
          }.hold(Optional.empty)
          timerSystem.at(alarm).map(_ => ())
        }
        out.filterMap {
          case Emit(v) => Some(v)
          case _ => None
        }
      })

    def calm(init: Lazy[Option[A]]): Stream[A] = stream.filterByPrevious((prev, next) => !prev.equals(next), init)

    def calm: Stream[A] = stream.calm(new Lazy(None))

    def throttle(timerSystem: SecondsTimerSystem, span: Double): Stream[A] =
      stream.bufferByTime(timerSystem, span, _.last)

  extension[A] (cell: Cell[A])
    def flatMap[B](f: A => Cell[B]): Cell[B] = Cell.switchC(cell.map(f(_)))

    def calm: Cell[A] =
      val init = cell.sampleLazy()
      Operational.updates(cell).calm(init.map(Some(_))).holdLazy(init)
