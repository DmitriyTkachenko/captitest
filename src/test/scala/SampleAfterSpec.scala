import captify.test.scala.TestAssignment.sampleAfter

class SampleAfterSpec extends BaseSpec {
  it should "return sample starting from the beginning" in {
    assert(sampleAfter(Seq[BigInt](1, 2, 3, 4, 5, 6, 7).toIterator, 0, 3).toList === List(1, 2, 3))
  }

  it should "return sample starting from the middle" in {
    assert(sampleAfter(Seq[BigInt](1, 2, 3, 4, 5, 6, 7).toIterator, 2, 4).toList === List(3, 4, 5, 6))
  }

  it should "return sample starting from the end" in {
    assert(sampleAfter(Seq[BigInt](1, 2, 3, 4, 5, 6, 7).toIterator, 6, 1).toList === List(7))
  }

  it should "work with zero-length sample" in {
    assert(sampleAfter(Seq[BigInt](1, 2, 3).toIterator, 1, 0).toList === List())
  }

  it should "work with sample of length 1" in {
    assert(sampleAfter(Seq[BigInt](1, 2, 3).toIterator, 1, 1).toList === List(2))
  }

  it should "return smaller sample if iterator does not have requested quantity" in {
    assert(sampleAfter(Seq[BigInt](1, 2, 3, 4, 5, 6).toIterator, 2, 10).toList === List(3, 4, 5, 6))
  }

  it should "work with redundant hasNext calls" in {
    val it = sampleAfter(Seq[BigInt](1, 2, 3, 4, 5, 6, 7).toIterator, 2, 2)
    3 times { assert(it.hasNext === true) }
    assert(it.next() === 3)
    2 times { assert(it.hasNext === true) }
    assert(it.next() === 4)
    4 times { assert(it.hasNext === false) }
  }

  it should "throw NoSuchElementException when next is invoked and hasNext returns false" in {
    val it = sampleAfter(Seq[BigInt](4, 5, 6, 7).toIterator, 1, 2)
    assert(it.hasNext === true)
    assert(it.next() === 5)
    assert(it.hasNext === true)
    assert(it.next() === 6)
    assert(it.hasNext === false)
    assertThrows[NoSuchElementException] {
      it.next()
    }
  }
}
