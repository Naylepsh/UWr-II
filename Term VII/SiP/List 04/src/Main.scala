import cards.{Ace, Card, Diamonds, Hearts, Numerical, Spades}
import deck.Deck
import games.Blackjack
import tests.{BlackjackTests, DeckTests}

object Main extends App {
  def first21(): Unit = {
    //    winning cards are: ace(11), ace(1), 9
    val cards = List(Card(Spades, Ace), Card(Hearts, Ace), Card(Spades, Numerical(9)), Card(Diamonds, Ace))
    val deck = new Deck(cards)
    val blackjack = new Blackjack(deck)

    blackjack.first21()
  }

  def blackjackPull(): Unit = {
    val blackjack = Blackjack()
    blackjack.play(4)
  }

  DeckTests.runTests()
  BlackjackTests.runTests()
  first21()
  println("------------")
  blackjackPull()
}


