package io.artkostm

import scala.deriving.*
import scala.compiletime.*

trait Functor[F[_]]:
    def fmap[A, B](f: A => B): F[A] => F[B]

    extension [A, B](fa: F[A])
        infix def map(f: A => B): F[B] = fmap(f)(fa)

object Functor:
    inline def apply[F[_]](using F: Functor[F]): Functor[F] = F

    def map[F[_]] =
        [A, B]                 =>
        (f: A => B)            =>
        (functor: Functor[F]) ?=>
        (fa: F[A])             => summon[Functor[F]].fmap(f)(fa)

    inline def summonAll[T <: Tuple, K[_[_]]]: List[K[[X] =>> Any]] =
        inline erasedValue[T] match
            case _: EmptyTuple => Nil
            case _: (t *: ts) => 
                summonInline[t].asInstanceOf[K[[X] =>> Any]] :: summonAll[ts, K]

    private def functorProduct[F[_], T](p: Kind1.Product[F], functors: => List[Functor[[X] =>> Any]]): Functor[F] =
        new Functor[F]:
            def fmap[A, B](f: A => B): F[A] => F[B] =
                (fa: F[A]) => {
                    val mapped = fa.asInstanceOf[Product].productIterator.zip(functors.iterator).map {
                        (fa, F) => F.fmap(f)(fa)
                    }
                    p.fromProduct(Tuple.fromArray(mapped.toArray)).asInstanceOf[F[B]]
                }

    private def functorSum[F[_]](s: Kind1.Sum[F], functors: => List[Functor[[X] =>> Any]]): Functor[F] =
        new Functor[F]:
            def fmap[A, B](f: A => B): F[A] => F[B] = 
                (fa: F[A]) => {
                    val index = s.ordinal(fa.asInstanceOf[s.MirroredMonoType])
                    functors(index).fmap(f)(fa).asInstanceOf[F[B]]
                }

    inline def derived[F[_]](using k: Kind1[F]): Functor[F] =
        lazy val functors = summonAll[LiftP[Functor, k.MirroredElemTypes], Functor]
        inline k match 
            case s: Kind1.Sum[F]     => functorSum(s, functors)
            case p: Kind1.Product[F] => functorProduct(p, functors)
        
    given Functor[Option] with
        def fmap[A, B](f: A => B): Option[A] => Option[B] = (oa: Option[A]) => oa.map(f)

    given [R]: Functor[R => *] with
        def fmap[A, B](f: A => B): (R => A) => (R => B) = (fa: R => A) => f.compose(fa)
    
    given [R]: Functor[Tuple2[R, *]] with
        def fmap[A, B](f: A => B): Tuple2[R, A] => (R, B) = (ta: (R, A)) => (ta._1, f(ta._2))
    
    given Functor[Id] with
        def fmap[A, B](f: A => B): A => B = (a: A) => f(a)

    given [Y[_], Z]: Functor[Const[Y[Z]]] with
        def fmap[A, B](f: A => B) = (ea: Y[Z]) => ea
