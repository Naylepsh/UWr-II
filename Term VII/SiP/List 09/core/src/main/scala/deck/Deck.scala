package deck

import cards._
import scala.util.Random.shuffle

class Deck(val cards: List[Card]) {
  def pull(): Deck = {
    cards match {
      case Nil => throw new PullFromEmptyDeckError()
      case _ => new Deck(cards.tail)
    }
  }

  def push(c: Card): Deck = new Deck(c :: cards)

  def push(color: Color, value: Rank): Deck = new Deck(Card(color, value) :: cards)

  val isStandard: Boolean = {
    val standardDeck = Deck.standardDeck
    cards.count(standardDeck contains _) == standardDeck.length
  }

  def duplicatesOfCard(card: Card): Int = {
    val occurrences = cards.count(_ == card)
    if (occurrences == 0) { 0 } else { occurrences - 1 }
  }

  def amountOfColor(suit: Color): Int = cards.count(_.color == suit)

  def amountOfNumerical(numerical: Numerical): Int = cards.count(_.rank == numerical)

  val amountWithNumerical: Int = cards.count(_.rank.isInstanceOf[Numerical])

  def amountOfFace(face: Face): Int = cards.count(_.rank == face)

  val amountWithFace: Int = cards.count(_.rank.isInstanceOf[Face])
}

object Deck {
  val standardDeck: List[Card] = {
    val colors = List(Clubs, Diamonds, Spades, Hearts)
    val ranks = List(Ace, Jack, Queen, King) ::: (for (rank <- (2 to 10).toList) yield Numerical(rank))
    val cards = for {
      color <- colors
      rank <- ranks
    } yield Card(color, rank)
    cards
  }

  def apply(): Deck = new Deck(shuffle(standardDeck))
}
