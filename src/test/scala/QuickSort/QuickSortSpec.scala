package org.ljw.qs

import org.specs2.mutable._
import org.specs2.mock.Mockito
import org.scalatest._

import org.ljw.qs._

class QuickSortSpec extends Specification {
  /*"test swap" in {
    class IQs extends ImperativeQs(_ => 0) with PrivateMethodTester {
      val swapPub = PrivateMethod[Unit]('swap)  // why is there no end "'"???
    }
    var a : Array[Int] = Array[Int](1, 2, 3)
    val iqs = new IQs
    iqs invokePrivate iqs.swapPub(a, 0, 2)
    a mustEqual Array[Int](3, 2 ,1)
  }*/

  "0 items" in {
    var emptyArray = Array[Int]()
    ImperativeQs.makeIQsZeroPivot.quickSort(emptyArray)
    emptyArray mustEqual Array()
  }

  "1 item" in {
    var oneItemArray = Array(2)
    ImperativeQs.makeIQsZeroPivot.quickSort(oneItemArray)
    oneItemArray mustEqual Array(2)
  }

  "simple" in {
    val qs = ImperativeQs.makeIQsZeroPivot
    var a = Array(2, 1)
    qs.quickSort(a)
    a mustEqual Array(1, 2)
  }

  "2, 1, 3" in {
    var threeItems = Array(2, 1, 3)
    val qs = ImperativeQs.makeIQsZeroPivot
    qs.quickSort(threeItems)
    threeItems mustEqual Array(1, 2, 3)
    qs.cmpCount mustEqual 2
  }

  "reverse order qs 3" in {
    val qs = ImperativeQs.makeIQsZeroPivot
    var reverse = Array.range(3, 0, -1)
    qs.quickSort(reverse)
    reverse mustEqual (1 to 3).toArray
  }

  "reverse order qs 4" in {
    val qs = ImperativeQs.makeIQsZeroPivot
    var reverse = Array.range(4, 0, -1)
    qs.quickSort(reverse)
    reverse mustEqual (1 to 4).toArray
  }

  "reverse order qs 4 far right pivot" in {
    val qs = ImperativeQs.makeIQsRightPivot
    var reverse = Array.range(4, 0, -1)
    qs.quickSort(reverse)
    reverse mustEqual (1 to 4).toArray
  }

  val testCaseVals = Array(3, 9, 8, 4, 6, 10, 2, 5, 7, 1)

  "forum test case zero pivot" in {
    var a = testCaseVals
    val qs = ImperativeQs.makeIQsZeroPivot
    qs.quickSort(a)
    a mustEqual (1 to 10).toArray
    qs.cmpCount mustEqual 25
  }

  "forum test case last pivot" in {
    var a = testCaseVals
    val qs = ImperativeQs.makeIQsRightPivot
    qs.quickSort(a)
    a mustEqual (1 to 10).toArray
    qs.cmpCount mustEqual 29
  }

  "mid of 4 is 1" in {
    ImperativeQs.getMidIndex(4) mustEqual 1
  }

  "mid of 3 is 1" in {
    ImperativeQs.getMidIndex(3) mustEqual 1
  }

  "mid of 9 is 4" in {
    ImperativeQs.getMidIndex(9) mustEqual 4
  }

  "mid of 10 is 4" in {
    ImperativeQs.getMidIndex(10) mustEqual 4
  }
}
