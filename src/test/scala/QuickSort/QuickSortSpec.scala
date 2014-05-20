package org.ljw.qs

import org.specs2.mutable._
import org.ljw.qs._

class QuickSortSpec extends Specification {
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
    val qs = new ImperativeQs(_ => 0)
    var a = Array(2, 1)
    qs.quickSort(a)
    a mustEqual Array(1, 2)
  }

  "reverse order qs" in {
    val qs = new ImperativeQs(_ => 0)
    var reverse = Array.range(4, 0, -1)
    qs.quickSort(reverse)
    reverse mustEqual (1 to 4).toArray
  }

}
