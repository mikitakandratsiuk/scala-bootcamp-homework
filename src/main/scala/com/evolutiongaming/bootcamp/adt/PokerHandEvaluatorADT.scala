package com.evolutiongaming.bootcamp.adt

sealed trait Suit

object Suit{
  final case object Hearts extends Suit
  final case object Diamonds extends Suit
  final case object Clubs extends Suit
  final case object Spades extends Suit
}

sealed trait Rank

object Rank {
  final case object RankTwo extends Rank
  final case object RankThree extends Rank
  final case object RankFour extends Rank
  final case object RankFive extends Rank
  final case object RankSix extends Rank
  final case object RankSeven extends Rank
  final case object RankEight extends Rank
  final case object RankNine extends Rank
  final case object RankTen extends Rank
  final case object RankJack extends Rank
  final case object RankQueen extends Rank
  final case object RankKing extends Rank
  final case object RankAce extends Rank
}

final case class Card(rank: Rank, suit: Suit)

sealed trait Hand

object Hand{
  final case class TexasHoldemHand(card1: Card, card2: Card) extends Hand
  final case class OmahaHoldemHand(card1: Card, card2: Card, card3: Card, card4: Card) extends Hand
}

final case class Board private(cards: Set[Card])

object Board{
  def of(cards: Set[Card]): Option[Board] = cards match {
    case cards if cards.size == 5 => Some(Board(cards))
    case _ => None
  }
}

sealed trait Combination

object Combination {
  final case class StraightFlush(topRank: Rank) extends Combination
  final case class FourKind(topRank: Rank, otherCardRank: Rank) extends Combination
  final case class FullHouse(trio: Rank, pair:Rank) extends Combination
  final case class Flush(firstRank: Rank, secondRank: Rank, thirdRank: Rank, fourthRank: Rank, fifthRank: Rank) extends Combination
  final case class Straight(topRank: Rank) extends Combination
  final case class ThreeOfAKind(topRank: Rank, otherCardRank1: Rank, otherCardRank2: Rank) extends Combination
  final case class TwoPairs(topRank: Rank, lowRank: Rank, otherCardRank: Rank) extends Combination
  final case class Pair(topRank: Rank, otherCardRank1: Rank, otherCardRank2: Rank, otherCardRank3: Rank) extends Combination
  final case class HighCard(topRank: Rank, otherCardRank1: Rank, otherCardRank2: Rank, otherCardRank3: Rank, otherCardRank4: Rank) extends Combination
}

import Hand._

sealed trait BoardWithHands

object BoardWithHands {
  final case class TexasHoldemCase(board: Board, hands: List[TexasHoldemHand])
  final case class OmahaHoldemCase(board: Board, hands: List[OmahaHoldemHand])
}

final case class TestResult(hand: Hand, combination: Combination)

class BoardEvaluator(board: Board, hands: List[Hand]) {
  def evaluateResult(board: Board, hands: List[Hand]): List[TestResult] = ???
}