import captify.test.scala.TestAssignment.mergeIterators

class MergeIteratorsSpec extends BaseSpec {
  it should "return elements from all given iterators" in {
    assert(mergeIterators(Seq[Iterator[BigInt]](
      Seq[BigInt](3, 5, 1).toIterator,
      Seq[BigInt](2, 7, 10).toIterator,
      Seq[BigInt](4, 0, 9).toIterator)).toSet === Set(0, 1, 2, 3, 4, 5, 7, 9, 10))
  }

  it should "not fail on empty iterators" in {
    assert(mergeIterators(Seq[Iterator[BigInt]](
      Iterator[BigInt](),
      Seq[BigInt](2, 7, 10).toIterator,
      Iterator[BigInt](),
      Seq[BigInt](4, 0, 9).toIterator,
      Iterator[BigInt]())).toSet === Set(0, 2, 4, 7, 9, 10))
  }

  it should "preserve ascending order and duplicates" in {
    assert(mergeIterators(Seq[Iterator[BigInt]](
      Iterator[BigInt](),
      Seq[BigInt](3, 3, 4).toIterator,
      Iterator[BigInt](),
      Seq[BigInt](1, 2, 3, 5, 7).toIterator,
      Seq[BigInt](4, 7, 10).toIterator,
      Iterator[BigInt]())).toList === List(1, 2, 3, 3, 3, 4, 4, 5, 7, 7, 10))
  }

  it should "work with redundant hasNext calls" in {
    val it = mergeIterators(Seq[Iterator[BigInt]](
      Iterator[BigInt](),
      Seq[BigInt](3, 3, 4).toIterator,
      Iterator[BigInt](),
      Seq[BigInt](1, 2, 3, 5, 7).toIterator,
      Seq[BigInt](4, 7, 10).toIterator,
      Iterator[BigInt]()))

    3 times { assert(it.hasNext === true) }
    assert(it.next() === 1)
    4 times { assert(it.hasNext === true) }
    assert(it.next() === 2)
    assert(it.toList === List(3, 3, 3, 4, 4, 5, 7, 7, 10))
    2 times { assert(it.hasNext === false) }
  }

  it should "throw NoSuchElementException when next is invoked and hasNext returns false" in {
    val it = mergeIterators(Seq[Iterator[BigInt]](
      Iterator[BigInt](),
      Seq[BigInt](1, 2).toIterator,
      Iterator[BigInt](),
      Seq[BigInt](3).toIterator,
      Iterator[BigInt]()))

    assert(it.toList === List(1, 2, 3))
    assert(it.hasNext === false)
    assertThrows[NoSuchElementException] {
      it.next()
    }
  }
}
