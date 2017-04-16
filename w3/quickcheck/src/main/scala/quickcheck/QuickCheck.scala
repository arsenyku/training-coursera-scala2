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

}
