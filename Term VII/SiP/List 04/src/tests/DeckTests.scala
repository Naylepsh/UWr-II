package tests

import cards._
import deck._

object DeckTests {
  def runTests(): Unit = {
    testPull()
    testStandardDeck()
    testDuplicateCards()
    testAmountOfColor()
    testAmountWithFace()
  }

  def testPull(): Unit = {
    testPullingOfTheFirstCard()
    testPullingFromEmptyDeck()
  }

  def testStandardDeck(): Unit = {
    val deck = Deck()

    assert(deck.isStandard)
  }

  def testDuplicateCards(): Unit = {
    testDuplicatesOfCardNotInDeck()
    testDuplicatesOfSingleCardInDeck()
    testDuplicatesOfNonUniqueCardInDeck()
  }

  def testAmountOfColor(): Unit = {
    val deck = Deck()
    val color = Spades

    val amountOfColor = deck.amountOfColor(color)

    val expectedAmount = 13
    assert(amountOfColor == expectedAmount)
  }

  def testAmountWithFace(): Unit = {
    val deck = Deck()

    val amountOfFace = deck.amountWithFace

    val colors = 4
    val facesPerColor = 3
    val expectedAmount = colors * facesPerColor
    assert(amountOfFace == expectedAmount)
  }

  def testPullingOfTheFirstCard(): Unit = {
    val ace = Card(Spades, Ace)
    val queen = Card(Hearts, Queen)
    val cards = List(ace, queen)
    val deck = new Deck(cards)

    val deckWithoutAce = deck.pull()

    assert(deckWithoutAce.cards.length == 1)
  }

  def testPullingFromEmptyDeck(): Unit = {
    val deck = new Deck(List())

    try {
      deck.pull()
      assert(assertion = false, "pulling from empty deck did not throw an error")
    } catch {
      case _: PullFromEmptyDeckError => ()
    }
  }

  def testDuplicatesOfCardNotInDeck(): Unit = {
    val deck = new Deck(List())
    val card = Card(Spades, Ace)

    val duplicates = deck.duplicatesOfCard(card)

    assert(duplicates == 0)
  }

  def testDuplicatesOfSingleCardInDeck(): Unit = {
    val card = Card(Spades, Ace)
    val deck = new Deck(List(card))

    val duplicates = deck.duplicatesOfCard(card)

    assert(duplicates == 0)
  }

  def testDuplicatesOfNonUniqueCardInDeck(): Unit = {
    val card = Card(Spades, Ace)
    val cards = List(card, card, card)
    val deck = new Deck(cards)

    val duplicates = deck.duplicatesOfCard(card)

    assert(duplicates == cards.length - 1)
  }
}
