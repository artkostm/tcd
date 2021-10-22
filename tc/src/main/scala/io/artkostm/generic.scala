package io.artkostm

import scala.deriving.*
import scala.compiletime.*


type Kind1[F[_]] = Mirror { type MirroredType[X] = F[X]; type MirroredElemTypes[_] <: Tuple }
object Kind1:
    type Sum[F[_]] = Mirror.Sum { type MirroredType[X] = F[X]; type MirroredElemTypes[_] <: Tuple }
    type Product[F[_]] = Mirror.Product { type MirroredType[X] = F[X]; type MirroredElemTypes[_] <: Tuple }

type LiftP[F[_[_]], T <: [X] =>> Tuple] <: Tuple =
  T[Any] match
    case a *: _ => F[[X] =>> Tuple.Head[T[X]]] *: LiftP[F, [X] =>> Tuple.Tail[T[X]]]
    case _      => EmptyTuple

type Const = [A] =>> [T] =>> A
type Id[X] = X