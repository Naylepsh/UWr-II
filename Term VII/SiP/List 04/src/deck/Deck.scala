package deck

import cards._
import scala.util.Random.shuffle

class Deck(val cards: List[Card]) {
  // creates new deck without first card
  def pull(): Deck = {
    cards match {
      case Nil => throw new PullFromEmptyDeckError()
      case _ => new Deck(cards.tail)
    }
  }

  // creates new deck with given card pushed on top
  def push(c: Card) = new Deck(c :: cards)

  // creates new deck with new card(color, value) pushed on top
  def push(color: Color, value: Rank) = new Deck(Card(color, value) :: cards)

  // checks if deck is a standard deck
  val isStandard: Boolean = {
    val standardDeck = Deck.standardDeck
    cards.count(standardDeck contains _) == standardDeck.length
  }

  // amount of duplicates of the given card in the deck
  def duplicatesOfCard(card: Card): Int = {
    val occurrences = cards.count(_ == card)
    if (occurrences == 0) { 0 } else { occurrences - 1}
  }

  // amount of cards in the deck for the given color
  def amountOfColor(suit: Color): Int = cards.count(_.color == suit)

  // amount of cards in the deck for given numerical card (2, 3, 4, 5, 6, 7, 8, 9, 10)
  def amountOfNumerical(numerical: Numerical): Int = cards.count(_.rank == numerical)

  // amount of all numerical cards in the deck (2, 3, 4, 5, 6, 7, 8, 9, 10)
  val amountWithNumerical: Int = cards.count(_.rank.isInstanceOf[Numerical])

  // amount of cards in the deck for the given face (Jack, Queen & King)
  def amountOfFace(face: Face) : Int = cards.count(_.rank == face)

  // amount of all cards in the deck with faces (Jack, Queen & King)
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

  // creates the standard deck with random order of cards.
  def apply() = new Deck(shuffle(standardDeck))
}
