package cards

sealed abstract class Color
case object Clubs extends Color
case object Diamonds extends Color
case object Spades extends Color
case object Hearts extends Color

sealed abstract class Rank
case object Ace extends Rank

case class Numerical(pips: Int) extends Rank {
  require(2 <= pips && pips <= 10)
}

sealed abstract class Face extends Rank
case object Jack extends Face
case object Queen extends Face
case object King extends Face

case class Card(color: Color, rank: Rank)
