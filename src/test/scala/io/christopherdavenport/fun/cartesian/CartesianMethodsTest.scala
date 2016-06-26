package io.christopherdavenport.fun.cartesian

import org.scalatest.{FlatSpec, Matchers}
import CartesianMethods._
/**
  * Created by davenpcm on 6/26/16.
  */
class CartesianMethodsTest extends FlatSpec with Matchers{

  "cartesianCombine" should "create the cross product of 2 sequences" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq(1, 2, 3, 4)
    assertResult(true){
      cartesianCombine(s1, s2)(_ + _) == Seq(
        Seq(2, 3, 4), Seq(3, 4, 5), Seq(4, 5, 6), Seq(5, 6, 7)
      )
    }
  }

  it should "work on arbitrary unmatching types" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq(1L, 2L, 3L, 4L)
    assertResult(true){
      cartesianCombine(s1, s2)(_ + _) == Seq(
        Seq(2, 3, 4), Seq(3, 4, 5), Seq(4, 5, 6), Seq(5, 6, 7)
      )
    }
  }

  it should "work for string interpolation" in {
    val s1 = Seq("He", "She", "Me")
    val s2 = Seq(1, 2, 3, 4)
    assertResult(true){
      cartesianCombine(s1, s2)(_ + _) == Seq(
        Seq("He1", "She1", "Me1"), Seq("He2", "She2", "Me2"), Seq("He3", "She3", "Me3"), Seq("He4", "She4", "Me4")
       )
    }
  }

  "unifyCombine" should "work equivalent to flatMap" in {
    def add(i1: Int, i2: Int) = i1 + i2
    val s1 = Seq(1, 2, 3)
    val s2 = Seq(1, 2, 3, 4)
    assertResult(true){
      unifyCombine(s1, s2)(add) == s2.flatMap { y => s1.map { x => x + y }}
    }
  }

  "tableIncrement" should "create a table without values not sequence" in {
    val s1 = Seq(1, 3, 5)
    val s2 = Seq(2, 4, 6)

    assertResult(true){
      TableIncrement(s1, s2)(_ * _) == Seq(
        Seq(" 2", "  ", " 6", "  ", "10"),
        Seq("  ", "  ", "  ", "  ", "  "),
        Seq(" 4", "  ", "12", "  ", "20"),
        Seq("  ", "  ", "  ", "  ", "  "),
        Seq(" 6", "  ", "18", "  ", "30")
      )
    }
  }

  "TableIncrementWithHeaders" should "create a table with all missing values of the range filled" in {
    val s1 = Seq(1, 3, 5)
    val s2 = Seq(2, 4, 6)

    assertResult(true){
      TableIncrementWithHeaders(s1, s2)(_ * _) ==Seq(
        Seq("  "," 1", " 2", " 3", " 4", " 5"),
        Seq(" 2"," 2", "  ", " 6", "  ", "10"),
        Seq(" 3","  ", "  ", "  ", "  ", "  "),
        Seq(" 4"," 4", "  ", "12", "  ", "20"),
        Seq(" 5","  ", "  ", "  ", "  ", "  "),
        Seq(" 6"," 6", "  ", "18", "  ", "30")
      )
    }
  }

}
