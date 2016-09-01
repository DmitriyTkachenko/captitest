package captify.test.scala

import captify.test.scala.SparseIterators._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Here are the functions to fill in.
 */
object TestAssignment {
  /**
   * Generate a contiguous sub-sample from given sequence.
   * 
   * Iterator provided should be immediately thrown away after calling this method, 
   * so don't worry about any side-effects.
   * 
   * @param iterator to be sampled
   * @param after the index of first element to be included, zero-based
   * @param sampleSize quantity of elements returned
   * @return sampleAfter(iteratorFromOne, 1, 2) should be same as to Seq[BigInt](2,3,4).toIterator 
   */
  def sampleAfter(iterator: Iterator[BigInt], after: Int, sampleSize: Int): Iterator[BigInt] = {
    require(after >= 0)
    require(sampleSize >= 0)
    new Iterator[BigInt] {
      private var position = 0
      private var elementsReturned = 0
      private var elementAvailable = false

      override def hasNext: Boolean = elementAvailable || {
        while (position < after && iterator.hasNext) {
          iterator.next()
          position += 1
        }
        elementAvailable = iterator.hasNext && elementsReturned < sampleSize
        elementAvailable
      }

      override def next(): BigInt = {
        if (hasNext) {
          elementsReturned += 1
          elementAvailable = false
          iterator.next()
        } else throw new NoSuchElementException
      }
    }
  }

  /**
   * Get value by index from given iterator.
   * 
   * Iterator provided should be immediately thrown away after calling this method, 
   * so don't worry about any side-effects.
   * 
   * @param iterator to get value from
   * @param position zero-based
   * @return value at given position
   */
  def valueAt(iterator: Iterator[BigInt], position: Int): BigInt = {
    require(position >= 0)
    var i = 0
    while (i != position && iterator.hasNext) {
      iterator.next()
      i += 1
    }
    if (iterator.hasNext) iterator.next()
    else null // could also throw IndexOutOfBoundsException or return Option
  }

  /**
   * Produce an iterator which generates values from given subset of input iterators.
   *
   * The iterator returned should conform to following properties:
   * * if incoming sequences are sorted ascending then output iterator is also sorted ascending
   * * duplicates are allowed:
   *   * if there're occurrences of the same value across multiple iterators - respective number of dupes are present in merged version
   *   * if there're any dupes present in one of input iterators - respective number of dupes are present in merged version
   *
   * @param iterators to be merged
   * @return Iterator with all elements and ascending sorting retained
   */
  def mergeIterators(iterators: Seq[Iterator[BigInt]]): Iterator[BigInt] = {
    new Iterator[BigInt] {
      private lazy val currentElements: Array[BigInt] = findFirstElements
      private var element: BigInt = _
      private var lastIteratorIndex: Int = -1

      private def findFirstElements: Array[BigInt] = {
        val elements = new Array[BigInt](iterators.size)
        for (i <- iterators.indices) advanceIterator(i, iterators, elements)
        elements
      }

      private def advanceIterator(iteratorIndex: Int, iterators: Seq[Iterator[BigInt]], elements: Array[BigInt]): Unit = {
        val it = iterators(iteratorIndex)
        if (it.hasNext) elements(iteratorIndex) = it.next()
        else elements(iteratorIndex) = null
      }

      override def hasNext: Boolean = element != null || {
        if (lastIteratorIndex != -1) advanceIterator(lastIteratorIndex, iterators, currentElements)
        lastIteratorIndex = -1

        val min: (BigInt, Int) = currentElements.zipWithIndex.reduceLeft((x, y) => {
          if (x._1 != null && y._1 != null) {
            if (x._1 <= y._1) x else y
          } else if (x._1 == null && y._1 != null) y
          else x
        })
        element = min._1
        lastIteratorIndex = min._2
        element != null
      }

      override def next(): BigInt = {
        if (!hasNext) throw new NoSuchElementException
        val e = element
        element = null
        e
      }
    }
  }

  /**
   * How much elements, on average, are included in sparse stream from the general sequence
   * @param sparsity to analyze
   * @param extent number of sequence elements to analyze
   * @return approximately 0.5 for sparsity=2, 0.33 for sparsity=3, and so on
   */
  def approximateSparsity(sparsity: Int, extent: Int): Double = {
    extent / valueAt(iteratorSparse(sparsity), extent - 1).toDouble
  }

  /**
   * Approximate actual for given range of sparsity values.
   *
   * As approximation is potentially long-running task, try to run calls to approximateSparsity() in parallel.
   * Also, as such calls may end up in exception for some tricky sparsity values, actual estimation should be kept in Try.
   *
   * For example, calling this with sparsityMin=2, sparsityMax=4, extent=1000 should:
   * - incur three calls to approximateSparsity for three respective values of sparsity and extent of 1000
   * - return Seq(2 -> Success(0.5), 3 -> Success(0.33), 4 -> Success(0.25)) (values given are approximates)
   *
   * @param sparsityMin non-negative value, inclusive for the range evaluated
   * @param sparsityMax non-negative value, inclusive for the range evaluated
   * @param extent this affects precision and time spent
   *
   * @return Seq of (Sparsity, Try[Approximation]) pairs
   */
  def approximatesFor(sparsityMin: Int, sparsityMax: Int, extent: Int): Seq[(Int, Try[Double])] = {
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
    val tasks: Seq[(Int, Future[Double])] = for (sparsity <- sparsityMin to sparsityMax)
      yield (sparsity, Future {
        approximateSparsity(sparsity, extent)
      })
    val futures: Seq[Future[Try[Double]]] = tasks.map(_._2).map(f => f.map(Success(_)).recover({ case e => Failure(e) }))
    Await.ready(Future.sequence(futures), Duration.Inf)
    tasks.map({ case (sparsity, future) => (sparsity, future.value.get) })
  }
}
