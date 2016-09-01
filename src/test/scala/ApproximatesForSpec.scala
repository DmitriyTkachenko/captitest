import captify.test.scala.TestAssignment.{approximateSparsity, approximatesFor}

class ApproximatesForSpec extends BaseSpec {
  it should "approximate sparsity" in {
    val results: Seq[(Int, Double)] = approximatesFor(2, 4, 1000000)
      .map({case (sparsity, result) => (sparsity, result.get)})
    assert(results(0)._1 === 2)
    results(0)._2 shouldBe (0.5 +- 0.01)
    assert(results(1)._1 === 3)
    results(1)._2 shouldBe (0.33 +- 0.01)
    assert(results(2)._1 === 4)
    results(2)._2 shouldBe (0.25 +- 0.01)
  }

  it should "return results, similar to single-thread implementation" in {
    val (extent, sparsityMin, sparsityMax) = (1000000, 2, 5)
    val singleThreadResults: Seq[(Int, Double)] = for (sparsity <- sparsityMin to sparsityMax)
      yield (sparsity, approximateSparsity(sparsity, extent))
    val concurrentResults: Seq[(Int, Double)] = approximatesFor(sparsityMin, sparsityMax, extent)
      .map({ case (sparsity, result) => (sparsity, result.get)})
    assert(singleThreadResults === concurrentResults)
  }
}
