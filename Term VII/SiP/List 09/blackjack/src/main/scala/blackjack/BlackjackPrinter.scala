package blackjack

import cards.Card

trait BlackjackPrinter {
  def prettyPrintCards(cards: List[Card], currentValue: Int = 0): Unit
  def prettyPrintHandValue(handValue: Int): Unit
}
