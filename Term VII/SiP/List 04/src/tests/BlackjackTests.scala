package tests

import cards._
import deck.Deck
import games.Blackjack

object BlackjackTests {
  def runTests(): Unit ={
    testAll21()
  }

  def testAll21(): Unit = {
//    winning cards are: ace(11), ace(1), 9 AND ace(11), 9, ace(1)
    val cards = List(Card(Spades, Ace), Card(Hearts, Ace), Card(Spades, Numerical(9)), Card(Diamonds, Ace))
    val deck = new Deck(cards)
    val blackjack = new Blackjack(deck)

    val all21 = blackjack.all21

    assert(all21.length == 2)
  }
}
