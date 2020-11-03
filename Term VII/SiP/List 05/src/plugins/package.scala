package object plugins {
  abstract class Pluginable {
    def plug(text: String): Option[String] = Option(text)
  }


  trait Reverting extends Pluginable {
    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(str.reverse)
      case None => None
    }
  }

  trait LowerCasing extends Pluginable {
    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(str.toLowerCase)
      case None => None
    }
  }

  trait SingleSpacing extends Pluginable {
    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(str.replaceAll(" +", " "))
      case None => None
    }
  }

  trait NoSpacing extends Pluginable {
    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(str.replaceAll(" +", ""))
      case None => None
    }
  }

  trait DuplicateRemoval extends Pluginable {
    private def removeDuplicates(chars: List[Char], text: String): String = chars match {
      case Nil => ""
      case char :: rest =>
        val charHasDuplicates = text.count(_ == char) > 1
        val x = if (charHasDuplicates) "" else char
        x + removeDuplicates(rest, text)
    }

    override def plug(text: String): Option[String] = Option(text) match {
      case Some(_) => super.plug(removeDuplicates(text.toList, text))
      case None => None
    }
  }

  trait Rotating extends Pluginable {
    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(str.takeRight(1) + str.take(str.length - 1))
      case None => None
    }
  }

  trait Doubling extends Pluginable {
    def doubleEverySecondChar(chars: List[Char]): String = repeatEveryNCharMTimes(chars, 2, 2)

    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(doubleEverySecondChar(str.toList))
      case None => None
    }
  }

  trait Shortening extends Pluginable {
    def removeEverySecondCharacter(chars: Array[Char]): String = removeEveryNthChar(chars, 2)

    override def plug(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plug(removeEverySecondCharacter(str.toCharArray))
      case None => None
    }
  }

  private def repeatEveryNCharMTimes(chars: List[Char], n: Int, m: Int): String = {
    def walk(chars: List[Char], index: Int): String = chars match {
      case Nil => ""
      case char :: rest =>
        val shouldRepeat = index % n == 0
        val charMultiplier = if (shouldRepeat) m else 1
        char.toString * charMultiplier + walk(rest, index + 1)
    }

    val initialIndex = 1
    walk(chars, initialIndex)
  }

/*
I'm assuming the example in pdf is wrong,
and deleting every second character from "ab cd" should return "a c" instead of "a d".
Otherwise there should be a separate counter for encountered alphanumeric characters
and removal should be based on that

I didn't really know what's a better approach to handling ifs inside match cases
that are "kinda-related-to-match-but-not-really" so I went with both ways
(the other way with if inside @repeatEveryNCharMTimes)
 */
  private def removeEveryNthChar(chars: Array[Char], n: Int): String = {
    val offset = 1
    def walk(index: Int): String = index match {
      case _ if index == chars.length => ""
      case _ if (index + offset) % n == 0 => walk(index + 1)
      case _ => chars(index) + walk(index + 1)
    }

    val initialIndex = 0
    walk(initialIndex)
  }
}
