import captify.test.scala.TestAssignment.valueAt

class ValueAtSpec extends BaseSpec {
  it should "work with empty iterator" in {
    assert(valueAt(Iterator[BigInt](), 0) === null)
  }

  it should "extract value at the beginning" in {
    assert(valueAt(Seq[BigInt](1, 2, 3, 4, 5).toIterator, 0) === 1)
  }

  it should "extract value at the middle" in {
    assert(valueAt(Seq[BigInt](1, 2, 3, 4, 5).toIterator, 2) === 3)
  }

  it should "extract value at the end" in {
    assert(valueAt(Seq[BigInt](1, 2, 3, 4, 5).toIterator, 4) === 5)
  }

  it should "not throw on overruning" in {
    assert(valueAt(Seq[BigInt](1, 2, 3, 4, 5).toIterator, 10) === null)
  }
}
