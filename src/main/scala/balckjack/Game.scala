package balckjack

import scala.annotation.tailrec
import scala.util.Random

case class Deck(cards: List[Card]) {
  def draw(drawSize: Int): (HandOf, Deck) = (HandOf(cards.take(drawSize):_*), Deck(cards.drop(drawSize)))
  def shuffle = Deck(Random.shuffle(cards))
}

object Deck {
  def apply() = new Deck(initialCards)

  private def initialCards: List[Card] = {
    import Rank._
    List.tabulate(4)(_ =>List(_2, _3, _4, _5, _6, _7, _8, _9, _10, Jack, Queen, King, Ace).map(Card(_))).flatten
  }
}

case class Game(deck: Deck, guest: Player, dealer: Player) {
  def playItOut: Option[Player] = {

    @tailrec
    def go(dk: Deck, guestHand: HandOf, dealerHand: HandOf): Option[Player] = {
      val (hand, newDeck) = dk.draw(1)
      dk.cards match {
        case _ if isGameLost(guestHand) => Some(dealer)
        case _ if isGameLost(dealerHand) => Some(guest)
        case _ if isBlackJack(guestHand) => Some(guest)
        case _ if isBlackJack(dealerHand) => Some(dealer)
        case x::ys if shouldContinue(guestHand) => go(newDeck, hand ::: guestHand, dealerHand)
        case x::ys if shouldContinue(dealerHand)  => go(newDeck, guestHand, hand ::: dealerHand)
        case _ if guestHand > dealerHand => Some(Sam)
        case _ if guestHand < dealerHand => Some(Dealer)
        case _ => None
      }
    }

    val (guestFirstHand, newDeck) = deck.draw(2)
    val (dealerFirstHand, newestDeck) = newDeck.draw(2)

    if (isBlackJack(guestFirstHand)) Some(guest)
    else if (isBlackJack(dealerFirstHand)) Some(dealer)
    else go(newestDeck, guestFirstHand, dealerFirstHand)
  }

  def isGameLost(hand: HandOf): Boolean = hand.value > 21
  def isBlackJack(hand: HandOf): Boolean = hand.value == 21
  def shouldContinue(hand: HandOf): Boolean = hand.value < 17
 }

case class HandOf(cards: Card*) extends Ordered[HandOf] {
  def :::(otherHand: HandOf): HandOf = HandOf(cards.toList ::: otherHand.cards.toList:_*)

  override def compare(that: HandOf): Int = value compare that.value

  def value: Int = cards.map(valueOf).sum

  import Rank._
  private def valueOf(card: Card) = card.rank match {
    case  Rank._2 => 2
    case  Rank._3 => 3
    case  Rank._4 => 4
    case  Rank._5 => 5
    case  Rank._6 => 6
    case  Rank._7 => 7
    case  Rank._8 => 8
    case  Rank._9 => 9
    case  Rank._10 | Jack | Queen | King => 10
    case  Ace => 11
  }
}

case class Card(rank: Rank)

sealed trait Rank
object Rank {
  case object _2 extends Rank
  case object _3 extends Rank
  case object _4 extends Rank
  case object _5 extends Rank
  case object _6 extends Rank
  case object _7 extends Rank
  case object _8 extends Rank
  case object _9 extends Rank
  case object _10 extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank
}

sealed trait Player
case object Sam extends Player
case object Dealer extends Player
