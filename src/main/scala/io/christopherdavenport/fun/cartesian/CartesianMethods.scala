package io.christopherdavenport.fun.cartesian

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * Created by davenpcm on 6/26/16.
  */
object CartesianMethods {

  def cartesianCombine[A,B,C](xValues: Seq[A], yValues: Seq[B])(f:  (A, B) => C): Seq[Seq[C]] = {
    for {
      y <- yValues
    } yield for {
      x <- xValues
    } yield {
      f(x, y)
    }
  }

  def unifyCombine[A,B,C](xValues: Seq[A], yValues: Seq[B])(f:  (A, B) => C): Seq[C] = {
    for {
      y <- yValues
      x <- xValues
    } yield f(x, y)
  }

  def prettyPrintCombineAll[A,B,C](xValues: Seq[A], yValues: Seq[B])(f:  (A, B) => C): Seq[Seq[String]] = {
    prettyPrintCombine(xValues, yValues)(f)((a, b) => true)
  }

  def prettyPrintCombine[A,B,C](xValues: Seq[A], yValues: Seq[B])
                          (f:  (A, B) => C)
                          (predicate: (A, B) => Boolean): Seq[Seq[String]] = {
    val GreatestLength = calculateLongestStringOf2SeqCombined(xValues, yValues)(f)

    cartesianCombine(xValues, yValues) { (a, b) =>
      if ( predicate(a, b) ) {
        val combine = f(a, b).toString
        val length = combine.length
        val diff = Math.abs(GreatestLength - length)
        List.fill(diff)(' ').mkString + combine.toString
      } else {
        List.fill(GreatestLength)(' ').mkString
      }

    }
  }

  private def calculateLongestStringOf2SeqCombined[A,B,C](xValues: Seq[A], yValues: Seq[B])(f: (A, B) => C) = {
    unifyCombine(xValues, yValues)(f).map(_.toString).maxBy(_.length).length
  }

  private def GenerateRange(seq: Seq[Int]): Range = {
    val min = seq.min
    val max = seq.max

    min to max
  }

  private def prettyPrintStringPad[A](seq: Seq[A], length: Int) = {
    for {
      a <- seq
    } yield {
      val diff = length - a.toString.length
      if (diff > 0) List.fill(diff)(' ').mkString + a.toString else a.toString
    }
  }

  def appendLeftAndUpperHeaders[A, B](upperHeader: Seq[A], leftHeader: Seq[B], table: Seq[Seq[String]]): Seq[Seq[String]] = {
    assert(upperHeader.length == table.head.length)
    assert(leftHeader.length == table.length)

    val LargestString = table.flatMap(_.map(_.length)).max


    val xStrings = prettyPrintStringPad(upperHeader, LargestString)
    val yStrings = prettyPrintStringPad(leftHeader, LargestString)

    val xHeader = { Seq.fill(LargestString)(' ').mkString } :: xStrings.toList

    appendUpperHeader(xHeader, appendLeftHeader(yStrings, table).toList)
  }

  def appendUpperHeader(appendUpper: Seq[String], grid: Seq[Seq[String]]): Seq[Seq[String]] = {
    assert(appendUpper.length == grid.maxBy(_.length).length)

    appendUpper :: grid.toList
  }

  def appendLeftHeader(appendLeft: Seq[String], grid: Seq[Seq[String]]): Seq[Seq[String]] = {
    assert(appendLeft.length == grid.length)

    @tailrec
    def recurseCombine(appendLeft: List[String],
                       grid: List[List[String]],
                       newGrid : List[List[String]] = List[List[String]]()): List[List[String]] = {
      (appendLeft, grid) match {
        case (x :: xs, y :: ys) =>
          val merge = x :: y
          recurseCombine(xs, ys, merge :: newGrid)
        case _ => newGrid.reverse
      }

    }

    recurseCombine(appendLeft.toList, grid.map(_.toList).toList)
  }

  def TableIncrement(xValues: Seq[Int], yValues: Seq[Int])
                    (f: (Int, Int) => Int) = {

    val xRange = GenerateRange(xValues)
    val yRange = GenerateRange(yValues)
    val xSet = xValues.toSet
    val ySet = yValues.toSet
    prettyPrintCombine(xRange, yRange)(f)((x, y) => xSet.contains(x) && ySet.contains(y))

  }

  def TableIncrementWithHeaders(xValues: Seq[Int], yValues: Seq[Int])
                               (f: (Int, Int) => Int)
  : Seq[Seq[String]] = {
    val xMin = xValues.min
    val yMin = yValues.min
    val xMax = xValues.max
    val yMax = yValues.max
    val xRange = xMin to xMax
    val yRange = yMin to yMax

    val cartesian = TableIncrement(xValues, yValues)(f)

    appendLeftAndUpperHeaders(xRange, yRange, cartesian)
  }

}
