/**
 * Created by ljw on 16/05/14.
 */

package org.ljw.qs

import scala.collection.immutable.Stream
import scala.collection.mutable



object Control {
  def using[A <: {def close() : Unit}, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }
}

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
  def makeIQsZeroPivot : ImperativeQs = new ImperativeQs(_ => 0)
  def fn(A: Array[Int]) : Int = A.size - 1
  def makeIQsRightPivot : ImperativeQs = new ImperativeQs(fn)
}

class ImperativeQs(pivot: Array[Int] => Int) {
  var cmpCount: Int = 0
  private def swap[T](A: Array[T], i: Int, j: Int) {
    val tmp = A(i)
    A(i) = A(j)
    A(j) = tmp
  }
  //private def swap[A, B](a: A, b: B): (B, A) = (b, a)

  private def partition(A: Array[Int], l: Int, r: Int) : Int = {
    println("partition " + l + "; " + r)
    val pivotIndex = pivot(A)
    if (pivotIndex != l)
      swap(A, pivotIndex, l) // pivot goes to first slot
    var i: Int = l + 1
    for (j <- l + 1 until r) {
      println("for " + j)
      if (A(j) < A(l)) {
        println("swap " + j + " : " + i)
        swap(A, j , i)  // element at are unknown
        i += 1
      }
    }
    swap(A, l, i - 1) // put the pivot back
    i - 1
  }

  private def quickSort(A: Array[Int], l: Int, r: Int) : Unit = {
    if (1 < r - l) {
      val pivot = partition(A, l, r)
      cmpCount += r - l - 1
      quickSort(A, l, pivot)
      quickSort(A, pivot + 1, r)
    }
  }
  def quickSort(A: Array[Int]) {
    quickSort(A, 0, A.size)
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