/**
 * Created by ljw on 16/05/14.
 */

package org.ljw.qs

import scala.collection.mutable
import org.ljw.util.Control

object ImperativeQs {
  def makeIQsZeroPivot : ImperativeQs = new ImperativeQs((_, x, _) => x)
  def makeIQsRightPivot : ImperativeQs = new ImperativeQs((_, _, y) => y)
  def calcMidIndex(length: Int) : Int = (length - 1)/2
  def getMediumIndex(A: Array[Int], left: Int, right: Int) : Int = {
    val midIndex : Int =  (right + left)/2
    val threeVals =  Array((left, A(left)), (right, A(right)), (midIndex, A(midIndex)))
    threeVals.sortWith((x, y) => x._2 < y._2)(1)._1 // return the index to the original Array
  }

  def makeIQsMedianPivot : ImperativeQs = new ImperativeQs((A, left, right) => {
    getMediumIndex(A, left, right)
  })
}

class ImperativeQs(pivot: (Array[Int], Int, Int) => Int) {
  var cmpCount: Int = 0
  private def swap[T](A: Array[T], i: Int, j: Int) {
    val tmp = A(i)
    A(i) = A(j)
    A(j) = tmp
  }
  //private def swap[A, B](a: A, b: B): (B, A) = (b, a)

  private def partition(A: Array[Int], left: Int, right: Int) : Int = {
    // partition over [left, right]
    val pivotIndex = pivot(A, left ,right)
    val p = A(pivotIndex)
    if (pivotIndex != left)
      swap(A, pivotIndex, left) // pivot goes to first slot
    var i: Int = left + 1
    for (j <- left + 1 to right) {
      if (A(j) < p) {
        swap(A, j , i)  // elements after i + 1 are unknown
        i += 1
      }
    }
    swap(A, left, i - 1) // put the pivot order
    i - 1
  }

  private def quickSort(A: Array[Int], left: Int, right: Int) : Unit = {
    if (left < right) {
      val pivot = partition(A, left, right)
      cmpCount += right - left
      quickSort(A, left, pivot - 1)
      quickSort(A, pivot + 1, right)
    }
  }

  def quickSort(A: Array[Int]) {
    quickSort(A, 0, A.size - 1)
  }
}

object Main extends App {
  var integerVals = mutable.ArrayBuffer[Int]()
  if (args.size > 0) {
    Control.using(io.Source.fromFile(args(0))) { source => {
      for (line <- source.getLines()) {
        integerVals += line.toInt
      }
    }
    }

    def getQuickSortCount(qs: ImperativeQs, a: Array[Int]) : Int = {
      qs.quickSort(a)
      qs.cmpCount
    }

    println("Zero privot Cmps: " + getQuickSortCount(ImperativeQs.makeIQsZeroPivot, integerVals.toArray))

    println("End pivot Cmps: " + getQuickSortCount(ImperativeQs.makeIQsRightPivot, integerVals.toArray))

    println("median of 3 pivot Cmps: " + getQuickSortCount(ImperativeQs.makeIQsMedianPivot, integerVals.toArray))
  }
}