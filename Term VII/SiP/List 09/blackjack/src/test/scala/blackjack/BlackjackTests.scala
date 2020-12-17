package blackjack

import cards._
import deck.Deck
import org.scalatest.funspec.AnyFunSpec

class BlackjackTests extends AnyFunSpec {
  describe("game creation") {
    it("should not allow usage of less than one deck") {
      val decksToUse = 0

      assertThrows[IllegalArgumentException](Blackjack(decksToUse))
    }

    it("should use a standard deck") {
      val game = Blackjack()
      val standardDeck = Deck()

      assert(game.deck.cards.length == standardDeck.cards.length)
    }

    it("should use all cards of all expected decks") {
      val decksToUse = 2
      val game = Blackjack(decksToUse)
      val standardDeck = Deck()

      assert(game.deck.cards.length == standardDeck.cards.length * decksToUse)
    }
  }

  describe("play") {
    it("should not allow to play less than one card") {
      val game = Blackjack()
      val cardsToPlay = 0

      assertThrows[IllegalArgumentException](game.play(cardsToPlay))
    }

    it("should pretty print cards") {
      val numOfDecks = 1
      val printer = BlackjackTestPrinter()
      val game = Blackjack(numOfDecks, printer)
      val cardsToPlay = 1

      game.play(cardsToPlay)

      assert(printer.prettyPrintCardsCalled)
    }

    it("should pretty print hand") {
      val numOfDecks = 1
      val printer = BlackjackTestPrinter()
      val game = Blackjack(numOfDecks, printer)
      val cardsToPlay = 1

      game.play(cardsToPlay)

      assert(printer.prettyPrintHandCalled)
    }
  }

  describe("all21") {
    it("should find all winning card combinations in a deck") {
      //    winning cards are: ace(11), ace(1), 9 AND ace(11), 9, ace(1)
      val cards = List(Card(Spades, Ace), Card(Hearts, Ace), Card(Spades, Numerical(9)), Card(Diamonds, Ace))
      val deck = new Deck(cards)
      val blackjack = new Blackjack(deck, BlackjackConsolePrinter)

      val all21 = blackjack.all21

      assert(all21.length == 2)
    }
  }

  describe("first21") {
    val numOfDecks = 1
    val printer = BlackjackTestPrinter()
    val game = Blackjack(numOfDecks, printer)

    game.first21()

    assert(printer.prettyPrintCardsCalled)
  }
}

case class BlackjackTestPrinter() extends BlackjackPrinter {
  var prettyPrintHandCalled = false
  var prettyPrintCardsCalled = false

  override def prettyPrintCards(cards: List[Card], currentValue: Int = 0): Unit = {
    prettyPrintCardsCalled = true
  }

  override def prettyPrintHandValue(handValue: Int): Unit = {
    prettyPrintHandCalled = true
  }
}

