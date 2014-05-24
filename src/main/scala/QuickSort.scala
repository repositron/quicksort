/**
 * Created by ljw on 16/05/14.
 */

package org.ljw.qs


import scala.collection.immutable.Stream
import scala.collection.mutable
import org.ljw.util.Control

class QuickSort(PivotFn: Stream[Int] => Int) {

}

object QuickSort {
  /*def quickSort(A: Stream, l: Int, r: Int) : Stream =
  {
    A
  }*/

  def quickSort(A: Stream[Int]) : Stream[Int] = A match {
    case p #:: xs => Stream.concat(quickSort(A.filter(_ < p)), Stream(p), quickSort(A.filter(p <= _)))
    case x #:: _ => Stream(x)
    //case _ => Stream.empty
  }
}

object ImperativeQs {
  def makeIQsZeroPivot : ImperativeQs = new ImperativeQs((_, x, _) => x)
  def makeIQsRightPivot : ImperativeQs = new ImperativeQs((_, _, y) => y)
  def getMidIndex(length: Int) : Int = (length - 1)/2


  def makeIQsMedianPivot : ImperativeQs = new ImperativeQs((A, left, right) => {
    val middleIndex : Int = (right - left - 1)/2 - 1
    val medianArray = Array(A(left), A(right), A(middleIndex))
    medianArray.sorted
    medianArray(1)
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
    if (pivotIndex != left)
      swap(A, pivotIndex, left) // pivot goes to first slot
    var i: Int = left + 1
    for (j <- left + 1 to right) {
      if (A(j) < A(left)) {
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
  }
}