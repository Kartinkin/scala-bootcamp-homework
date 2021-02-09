package com.evolutiongaming.bootcamp.basics

object LeetCode {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
      def step(prev: Int, nums: List[Int]): List[Int] = nums match {
          case Nil => Nil
          case x :: xs => (x + prev) :: step(x + prev, xs)
      }
      step(0, nums.toList).toArray
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] =
      nums.slice(0, n).lazyZip(nums.drop(n)).flatten { case (x, y) => Array(x, y) }.toArray

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int =
      accounts.map(_.sum).max

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
      val m = candies.max - extraCandies
      candies.map(_ >= m)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
      val s = points.map(_(0)).sorted
      s.drop(1).view.foldLeft((0, s(0))) {
          case ((width, prev), x) => (width.max(x - prev), x)
      }._1
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int =
    s.foldLeft((0,0)) {
        case ((depth, opened), '(') => (depth, opened + 1)
        case ((depth, opened), ')') => (depth.max(opened), opened - 1)
        case (acc, _) => acc
    }._1

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
      s.foldLeft((0, 0, 0)) {
          case ((b, l, r), 'R') => if (l>1) (b, l-1, 0) else if (l==1) (b+1, 0, 0) else (b, 0, r+1)
          case ((b, l, r), 'L') => if (r>1) (b, 0, r-1) else if (r==1) (b+1, 0, 0) else (b, l+1, 0)
          case (acc, _) => acc
      }._1
  }

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
      val m = mat.length
      val n = mat(0).length
      val K1 = K + 1
      val temp = mat.map { row =>
          (0 until n).map(i => row.slice(i-K, i+K1).view.sum).toArray
      }
      (0 until m).toArray.map { j:Int =>
          temp.slice(j-K, j+K1).view.reduce {
              (acc, row) => acc.lazyZip(row).map { case (x, y) => x+y }.toArray
          }
      }
  }

}


object DataStructures {

  val vegetables = Set("tomatoes", "peppers", "pumpkins", "cucumbers", "olives")

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  val totalVegetableCost: Int =
     vegetableAmounts
      .map { case (vegetable, amount) => vegetablePrices.getOrElse(vegetable, 10) * amount }
      .sum

  val totalVegetableWeights: Map[String, Int] =
    for {
      (vegetable, amount) <- vegetableAmounts
      weight <- vegetableWeights.get(vegetable)
    } yield (vegetable, amount * weight)


  def allEqual[T](list: List[T]): Boolean =
    list.toSet.size <= 1

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = n match {
    case 1 => set.map(x => Set(x))
    case n if n > 1 =>
      for {
        x <- set
        subset <- allSubsetsOfSizeN(set - x, n - 1)
      } yield subset + x
    case default => Set()
  }

  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] =
    map.groupBy { case (_, x) => x }
      .map { case (x, elements) => (elements.keySet, x) }
      .toList.sortBy { case (s, x) => x }
}
