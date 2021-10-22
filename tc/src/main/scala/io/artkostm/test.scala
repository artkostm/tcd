package io.artkostm

import Show.*
import Functor.*

final case class TestCaseClass(
    intField: Int,
    stringField: String,
    doubleField: Double,
    optIntField: Option[Int]
) derives Show

enum TestEnum(i: Int, d: Double) derives Show:
    case First(b: Int, c: Double, x: String) extends TestEnum(b, c)
    case Second(i: Int) extends TestEnum(i, 2.0)


final case class TestNested(e: TestEnum, name: String) derives Show

final case class TestContainer[A](elem: A) derives Functor

@main def testTypeclassDerivation =

    val tcc = TestCaseClass(1, "2", 3.0, Some(4))
    

    println(Show[TestCaseClass].show(tcc))
    println(Show[TestEnum].show(TestEnum.First(1, 1.0, "1")))
    println(Show[TestEnum.Second].show(TestEnum.Second(2)))
    println("it's working")
    println(TestNested(TestEnum.Second(2), "nested").show)

    println(TestContainer[Int](10).map(_ + 5))
    println(TestContainer(Option(10)).map(_.flatMap(i => Some(i + 11))))