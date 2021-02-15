package com.evolutiongaming.bootcamp.adt

import scala.annotation.switch

object Poker {

  sealed abstract class CardError(message: String)
  object CardError {
    case object BadSuit extends CardError("Bad suit")
    case object BadRank extends CardError("Bad rank")
    case object BadHand extends CardError("Bad hand")
    case object BadBoard extends CardError("Bad board")
  }

  sealed trait Suit
  object Suit {
    final case object Heart extends Suit
    final case object Diamond extends Suit
    final case object Club extends Suit
    final case object Spade extends Suit

    def apply(s: Char): Either[CardError, Suit] = (s: @switch) match {
      case 'h' => Right(Heart)
      case 'd' => Right(Diamond)
      case 'c' => Right(Club)
      case 's' => Right(Spade)
      case default => Left(CardError.BadSuit)
    }
  }

  sealed trait Rank
  object Rank {
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank

    def apply(r: Char): Either[CardError, Rank] = (r: @switch) match {
      case '2' => Right(Two)
      case '3' => Right(Three)
      case '4' => Right(Four)
      case '5' => Right(Five)
      case '6' => Right(Six)
      case '7' => Right(Seven)
      case '8' => Right(Eight)
      case '9' => Right(Nine)
      case 'T' => Right(Ten)
      case 'J' => Right(Jack)
      case 'Q' => Right(Queen)
      case 'K' => Right(King)
      case 'A' => Right(Ace)
      case default => Left(CardError.BadRank)
    }
  }


  final case class Card(suit: Suit, rank: Rank)

  sealed abstract class CardSet private (val cards: List[Card])
  object CardSet {
    case class Board(override val cards: List[Card]) extends CardSet(cards) {
      def apply(cards: List[Card]): Either[CardError, Board] =
        if (cards.size != 5) Left(CardError.BadBoard)
        else Right(new Board(cards))
    }
    case class TexasHand(override val cards: List[Card]) extends CardSet(cards) {
      def apply(cards: List[Card]): Either[CardError, TexasHand] =
        if (cards.size != 2) Left(CardError.BadHand)
        else Right(new TexasHand(cards))
    }
    case class OmahaHand(override val cards: List[Card]) extends CardSet(cards) {
      def apply(cards: List[Card]): Either[CardError, OmahaHand] =
        if (cards.size != 4) Left(CardError.BadHand)
        else Right(new OmahaHand(cards))
    }
    case class Combination private(override val cards: List[Card]) extends CardSet(cards) {}
    object Combination {
      def apply(board: List[Card], hand: List[Card]): Combination = new Combination(board ::: hand)
      def apply(list: List[Card]): Combination = new Combination(list)
    }
  }

  sealed trait PokerCombination extends Ordered[PokerCombination] {
    def compare(that: PokerCombination): Int = ???
  }
  object PokerCombination {
    def apply(cards: CardSet.Combination): PokerCombination = ???
    final case object StraightFlush extends PokerCombination
    final case object FourOfAKind extends PokerCombination
    final case object FullHouse extends PokerCombination
    final case object Flush extends PokerCombination
    final case object Straight extends PokerCombination
    final case object ThreeOfAKind extends PokerCombination
    final case object TwoPairs extends PokerCombination
    final case object Pair extends PokerCombination
    final case object HighCard extends PokerCombination
  }

  sealed abstract class TestCase {
    def rate: PokerCombination
  }
  object TestCase {
    case class OmahaCase(board: CardSet.Board, hand: CardSet.OmahaHand) extends TestCase {
      def rate: PokerCombination =
        (for {
          b <- board.cards.combinations(3)
          h <- hand.cards.combinations(2)
        } yield PokerCombination(CardSet.Combination(b, h))).max
    }

    case class TexasCase(board: CardSet.Board, hand: CardSet.TexasHand) extends TestCase {
      def rate: PokerCombination =
        (for {
          c <- CardSet.Combination(board.cards, hand.cards).cards.combinations(5)
        } yield PokerCombination(CardSet.Combination(c))).max
    }
  }

}
