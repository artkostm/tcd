package io.artkostm

import scala.deriving.*
import scala.compiletime.*

trait Show[A]:
  def show(a: A): String

object Show:
    extension[A: Show] (a: A) def show: String = Show[A].show(a)

    inline def apply[A](using s: Show[A]): Show[A] = s

    private def showProduct[T](shows: => List[Show[?]], name: String): Show[T] =
        new Show[T]:
            def show(t: T): String =
                val p = t.asInstanceOf[Product]
                p.productIterator.zip(shows.iterator).zipWithIndex.map { (pair, i) =>
                    val (v, s) = pair
                    s"${p.productElementName(i)}=${s.asInstanceOf[Show[Any]].show(v)}"
                }.mkString(s"$name(", ", ", ")")

    private def showSum[T](s: Mirror.SumOf[T], shows: => List[Show[?]]): Show[T] =
        new Show[T]:
            def show(t: T): String = 
                val index = s.ordinal(t)
                shows(index).asInstanceOf[Show[Any]].show(t)

    inline def summonAll[T <: Tuple]: List[Show[?]] =
        inline erasedValue[T] match
            case _: EmptyTuple => Nil
            case _: (t *: ts)  => summonInline[Show[t]] :: summonAll[ts]

    inline given derived[T](using m: Mirror.Of[T]): Show[T] =
        lazy val shows = summonAll[m.MirroredElemTypes]
        lazy val name = constValue[m.MirroredLabel]
        inline m match
            case s: Mirror.SumOf[T] => showSum(s, shows)
            case _: Mirror.ProductOf[T] => showProduct(shows, name)
    
    given Show[String] with
        def show(s: String): String = s"String($s)"
    
    given Show[Int] with
        def show(i: Int): String = s"Int($i)"
    
    inline given Show[Double] with
        def show(d: Double): String = s"Double($d)"

    given[T](using S: Show[T]): Show[Option[T]] = 
        new Show[Option[T]]:
            def show(opt: Option[T]): String =
                opt match 
                    case Some(t) => s"Option(${S.show(t)})"
                    case None    => "None"
