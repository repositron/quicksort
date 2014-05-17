/**
 * Created by ljw on 16/05/14.
 */
class QuickSort {

}

object QuickSort {
  def quickSort(A: Stream, l: Int, r: Int) : Stream =
  {
    A
  }
  def quickSort(A: Stream, pivotFn: Stream => Int) : Stream = A match {
    case x #:: Nil => Stream(x)

  }
}
