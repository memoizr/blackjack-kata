package blackjack

import balckjack._
import org.scalatest.prop.TableDrivenPropertyChecks
import Rank._

class GameFeatureTest extends BaseTest with TableDrivenPropertyChecks {

  "A Deck" - {
    "should have 52 cards" in {
      val deck = Deck()
      deck.cards.size shouldBe 52
    }

    "can be shuffled" in {
      val deck = Deck()

      deck.shuffle.cards should not be deck.cards
    }

    "lets you draw some cards" in {
      val deck = Deck()
      val drawSize: Int = 2
      val (hand, newDeck) = deck.draw(drawSize)

      hand shouldBe HandOf(deck.cards.take(drawSize):_*)
      newDeck.cards.size shouldBe deck.cards.size - drawSize
    }
  }

  "A hand of cards" - {
    "should have appropriate values" in {
      val table = Table(
        ("rank", "value"),
        (_2, 2),
        (_3, 3),
        (_4, 4),
        (_5, 5),
        (_6, 6),
        (_7, 7),
        (_8, 8),
        (_9, 9),
        (_10, 10),
        (Jack, 10),
        (Queen, 10),
        (King, 10),
        (Ace, 11)
      )

      forAll(table) { (rank, value) =>
        HandOf(Card(_2), Card(rank)).value shouldBe 2 + value
        HandOf(Card(Ace), Card(rank)).value shouldBe 11 + value
      }
    }
  }


  "A game" - {
    "is won by guest if they get blackjack at first round" in {
      val game = gameOf(handOf21 ::: handOf20)

      game.playItOut shouldBe Some(Sam)
    }


    "is won by Dealer if they get blackjack at first round" in {
      val game = gameOf(handOf20 ::: handOf21)

      game.playItOut shouldBe Some(Dealer)
    }

    "is lost if guest draws more than 21" in {
      val game = gameOf(handOf22 ::: handOf20)

      game.playItOut shouldBe Some(Dealer)
    }

    "is lost if Dealer draws more than 21" in {
      val game = gameOf(handOf20 ::: handOf22)

      game.playItOut shouldBe Some(Sam)
    }

    "guest picks more card after first round and wins if gets blackjack" in {
      val game = gameOf(handOf16 ::: handOf5 ::: handOf5 ::: handOf3)

      game.playItOut shouldBe Some(Sam)
    }

    "guest stops picking at 17" in {
      val game = gameOf(handOf17 ::: handOf16 ::: handOf6)

      game.playItOut shouldBe Some(Sam)
    }

    "dealer picks more card after guest round and wins if gets blackjack" in {
      val game = gameOf(handOf16 ::: handOf16 ::: handOf3 ::: handOf5 ::: handOf3)

      game.playItOut shouldBe Some(Dealer)
    }

    "player closest to 17 wins" in {
      val gameWonBySam = gameOf(handOf16 ::: handOf16 ::: handOf4 ::: handOf3)

      gameWonBySam.playItOut shouldBe Some(Sam)

      val gameWonByDealer = gameOf(handOf16 ::: handOf16 ::: handOf3 ::: handOf4)

      gameWonByDealer.playItOut shouldBe Some(Dealer)
    }

    "should draw if cards run out with even scores" in {
      val game = gameOf(handOf16 ::: handOf16)

      game.playItOut shouldBe None
    }

    "should draw if with even scores" in {
      val game = gameOf(handOf16 ::: handOf16 ::: handOf3 ::: handOf3 ::: handOf3 ::: handOf3)

      game.playItOut shouldBe None
    }

    def gameOf(cards: List[Card]): Game = {
      val deck = Deck(cards)
      Game(deck, Sam, Dealer)
    }
  }

  def handOf22 = List(Ace, Ace).map(Card(_))
  def handOf21 = List(_10, Ace).map(Card(_))
  def handOf20 = List(_10, _10).map(Card(_))
  def handOf17 = List(_10, _7).map(Card(_))
  def handOf16 = List(_10, _6).map(Card(_))
  def handOf15 = List(_9, _6).map(Card(_))
  def handOf6 = List(_6).map(Card(_))
  def handOf5 = List(_5).map(Card(_))
  def handOf4 = List(_4).map(Card(_))
  def handOf3 = List(_3).map(Card(_))
}
