package blackjack

import blackjack.Blackjack.cardValue
import cards.Card
import scala.annotation.tailrec

object BlackjackConsolePrinter extends BlackjackPrinter {
  @tailrec
  final override def prettyPrintCards(cards: List[Card], currentValue: Int = 0): Unit = cards match {
    case Nil => ()
    case card :: otherCards =>
      val value = cardValue(card, currentValue)
      prettyCardPrint(card, value)
      prettyPrintCards(otherCards, currentValue + value)
  }

  def prettyCardPrint(card: Card, cardValue: Int): Unit = {
    println(s"${card.rank} of ${card.color} [$cardValue]")
  }

  override def prettyPrintHandValue(handValue: Int): Unit = {
    println(s"Hand value: $handValue")
  }
}
