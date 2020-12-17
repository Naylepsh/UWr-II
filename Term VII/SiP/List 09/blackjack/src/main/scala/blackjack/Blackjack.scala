package blackjack

import cards._
import deck._
import scala.annotation.tailrec

class Blackjack(deck: Deck) {
  import Blackjack._
  def play(n: Int): Unit = {
    require(n > 0)

    val cards = deck.cards.take(n)
    val handValue = evaluateHand(cards)
    prettyCardsPrint(cards)
    println(s"Hand value: $handValue")
  }

  lazy val all21: List[List[Card]] = {
    val deckSize = deck.cards.length
    (for {
      from <- 0 to deckSize
      until <- from + 1 to deckSize
      cards = deck.cards.slice(from, until)
      if evaluateHand(cards) == 21
    } yield cards).toList
  }

  def first21(): Unit = {
    val hand = all21.head
    prettyCardsPrint(hand)
  }
}

object Blackjack {
  val bust = 21
  val aceMinValue = 1
  val aceMaxValue = 11

  def apply(numOfDecks: Int = 1): Blackjack = {
    import scala.util.Random.shuffle
    require(numOfDecks > 0)

    def getEnoughCardsForDecks(n: Int): List[Card] = {
      val deck = Deck.standardDeck
      if (n == 1) { deck } else { deck ::: getEnoughCardsForDecks(n - 1) }
    }

    val cards = getEnoughCardsForDecks(numOfDecks)
    val deck = new Deck(shuffle(cards))
    new Blackjack(deck)
  }

  @tailrec
  def evaluateHand(cards: List[Card], currentValue: Int = 0): Int = cards match {
    case Nil => currentValue
    case card :: otherCards =>
      val value = cardValue(card, currentValue)
      evaluateHand(otherCards, currentValue + value)
  }

  @tailrec
  def prettyCardsPrint(cards: List[Card], currentValue: Int = 0): Unit = cards match {
    case Nil => ()
    case card :: otherCards =>
      val value = cardValue(card, currentValue)
      prettyCardPrint(card, value)
      prettyCardsPrint(otherCards, currentValue + value)
  }

  def prettyCardPrint(card: Card, cardValue: Int): Unit = {
    println(s"${card.rank} of ${card.color} [$cardValue]")
  }

  def cardValue(card: Card, currentValue: Int): Int = {
    val faceCardValue = 10
    card.rank match {
      case numerical: Numerical => numerical.pips
      case _: Face => faceCardValue
      case _: Ace.type => bestAceValue(currentValue)
    }
  }

  def bestAceValue(currentValue: Int): Int = {
    if (currentValue + aceMaxValue < bust) aceMaxValue else aceMinValue
  }
}
