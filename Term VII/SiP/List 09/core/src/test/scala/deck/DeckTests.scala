package deck

import org.scalatest.funspec.AnyFunSpec
import cards._

class DeckTests extends AnyFunSpec {
  describe("pull") {
    it("should failed when pulled from empty deck") {
      val cards = List()
      val emptyDeck = new Deck(cards)

      assertThrows[PullFromEmptyDeckError](emptyDeck.pull())
    }

    it("should remove the pulled card") {
      val deckBeforePull = Deck()

      val deckAfterPull = deckBeforePull.pull()

      assert(deckBeforePull.cards.length == deckAfterPull.cards.length + 1)
    }
  }

  describe("push") {
    describe("a created card") {
      it("should add that card to a deck") {
        val deck = Deck()
        val card = Card(Spades, Ace)

        val deckAfterPush = deck.push(card)

        assert(deckAfterPush.cards.length == deck.cards.length + 1)
      }
    }

    describe("and create a card") {
      it("should create and push a card to a deck") {
        val deck = Deck()

        val deckAfterPush = deck.push(Spades, Ace)

        assert(deckAfterPush.cards.length == deck.cards.length + 1)
      }
    }
  }

  describe("standard deck") {
    it("should be created by Deck companion object") {
      val deck = Deck()
      assert(deck.isStandard)
    }
  }

  describe("duplicates of a card") {
    it("should find no duplicates in an empty deck") {
      val cards = List()
      val emptyDeck = new Deck(cards)
      val card = Card(Spades, Ace)

      val duplicates = emptyDeck.duplicatesOfCard(card)

      assert(duplicates == 0)
    }

    it("should find no duplicates in a deck with only one card") {
      val card = Card(Spades, Ace)
      val cards = List(card)
      val emptyDeck = new Deck(cards)

      val duplicates = emptyDeck.duplicatesOfCard(card)

      assert(duplicates == 0)
    }

    it("should find all duplicates of a non-unique card in a deck") {
      val card = Card(Spades, Ace)
      val cards = List(card, card, card)
      val emptyDeck = new Deck(cards)

      val duplicates = emptyDeck.duplicatesOfCard(card)

      assert(duplicates == cards.length - 1)
    }
  }

  describe("amount of a color") {
    it("should find all cards of a color in a deck") {
      val deck = Deck()

      val found = deck.amountOfColor(Spades)
      val expected = 13

      assert(found == expected)
    }
  }

  describe("amount of numerical") {
    it("should find all cards with the same number of pips in a deck") {
      val deck = Deck()
      val pips = 6
      val card = Numerical(pips)

      val found = deck.amountOfNumerical(card)

      val expected = 4
      assert(found == expected)
    }
  }

  describe("amount of face") {
    it("should find all cards of a face in a deck") {
      val deck = Deck()
      val face = King

      val found = deck.amountOfFace(face)

      val expected = 4
      assert(found == expected)
    }
  }

  describe("amount with numerical") {
    it("should find all numerical cards in a deck") {
      val deck = Deck()

      val numerical = deck.amountWithNumerical

      val colors = 4
      val numericalPerColor = 9
      val expected = colors * numericalPerColor
      assert(numerical == expected)
    }
  }

  describe("amount with face") {
    it("should find all face cards in a deck") {
      val deck = Deck()

      val amountOfFace = deck.amountWithFace

      val colors = 4
      val facesPerColor = 3
      val expectedAmount = colors * facesPerColor
      assert(amountOfFace == expectedAmount)
    }
  }
}
