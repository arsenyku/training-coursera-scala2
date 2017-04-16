package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  self =>

  lazy val genHeap: Gen[H] = oneOf(
    const(self.empty),
    for {
      k <- arbitrary[A]
      m <- oneOf(const(self.empty), genHeap)
    } yield self.insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap
  // should get the smallest of the two elements back.
  property("minOf2") =
  {
    val m = 749
    val expectedMin = 92
    val twoElementHeap = insert(expectedMin, insert(m, self.empty))
    findMin(twoElementHeap) == expectedMin
  }

  property("correctMin") = forAll
  { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val expectedMin = m - 1
    findMin(insert(expectedMin, h)) == expectedMin
  }


  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should
  // be empty.
  property("deleteLast") = forAll
  { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val expectedMin = m - 1
    findMin(insert(expectedMin, h)) == expectedMin
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and
  // deleting minima. (Hint: recursion and helper functions are your friends.)

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.


}
