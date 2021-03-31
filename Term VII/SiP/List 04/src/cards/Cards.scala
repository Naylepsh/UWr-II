package cards

sealed abstract class Color
case object Clubs extends Color
case object Diamonds extends Color
case object Spades extends Color
case object Hearts extends Color

sealed abstract class Rank
// could also create a 'sealed abstract class Ace' that case object Ace would derive from
// that would allow usage of 'case _: Ace' (instead of '... Ace.type') but that's one more 'Ace'
// don't know which is more preferable, so I'm leaving it as is
//SIP Both options are OK
case object Ace extends Rank

case class Numerical(pips: Int) extends Rank {
  require(2 <= pips && pips <= 10)
}

sealed abstract class Face extends Rank
case object Jack extends Face
case object Queen extends Face
case object King extends Face

case class Card(color: Color, rank: Rank)
