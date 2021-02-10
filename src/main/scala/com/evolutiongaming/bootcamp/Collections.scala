package com.evolutiongaming.bootcamp

object CollectionsLeetcode extends App {
  def runningSum(nums: Array[Int]): Array[Int] = nums.scanLeft(0)(_ + _).tail

  def shuffle(nums: Array[Int], n: Int): Array[Int] = (0 until n)
    .flatMap(element => Array(nums(element), nums(element + n)))
    .toArray

  def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.map(_.sum).max

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val currentMax = candies.max
    candies.map(candy => candy + extraCandies >= currentMax)
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xCoordinates = points.map(_.head).sorted
    xCoordinates.zip(xCoordinates.tail).map { case (x1, x2) => x2 - x1 }.max
  }
}

object Collections extends App {
  def findGap(l: List[Int]): Option[(Int, Int)] = l match {
    case Nil => None
    case _ :: xs => (l zip xs).find { case (x, y) => y - x != 1 }
  }

  def min(list: List[Int]): Option[Int] = list match {
    case Nil => None
    case _ => Some(list.foldLeft(list.head)(_ min _))
  }

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = list
    .foldLeft(List(zero))((acc, elem) => f(acc.head, elem) :: acc).reverse

  def count(s: String): List[(Char, Int)] = {
    s.toList.foldRight(List.empty[(Char, Int)]){
      case (char, (previousChar, cnt) :: xs) if previousChar == char => (char, cnt + 1) :: xs
      case (char, list) => (char, 1) :: list
    }
  }
}