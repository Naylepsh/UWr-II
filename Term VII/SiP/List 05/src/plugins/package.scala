package object plugins {
/*
  the easiest way would be to have Puginable look like this
  abstract class Pluginable {
    def plugin(text: String): Option[String] = Option(text)
  }

  Then for every trait we just repeat case Some(), case None, like this:
  trait Reverting extends Pluginable {
    override def plugin(text: String): Option[String] = Option(text) match {
      case Some(str) => super.plugin(str.reverse)
      case None => None
    }
  }

  However, that feels like too much of boilerplate code, hence why the helper below.
  Unfortunately, it still requires passing super.plugin as an argument, but I'd say it's a huge improvement over case spam
*/
  //SIP Could have a bit simpler architecture, like:
  //trait Plugin {
  //  def plugin(s: String): String = s
  //}
  //trait LowerCasing extends Plugin {
  //  override def plugin(s: String): String = super.plugin(s.toLowerCase)
  //}
  //...

  def plug(transform: String => String, next: String => Option[String]): String => Option[String] = {
    (text: String) => Option(text) match {
      case Some(str) => next(transform(str))
      case None => None
    }
  }

  trait Pluginable {
    def plugin: String => Option[String] = (text: String) => Option(text)
  }

  trait Reverting extends Pluginable {
    override def plugin: String => Option[String] = plug(_.reverse, super.plugin)
  }

  trait LowerCasing extends Pluginable {
    override def plugin: String => Option[String] = plug(_.toLowerCase, super.plugin)
  }

  trait SingleSpacing extends Pluginable {
    override def plugin: String => Option[String] = plug(_.replaceAll(" +", " "), super.plugin)
  }

  trait NoSpacing extends Pluginable {
    override def plugin: String => Option[String] = plug(_.replaceAll(" +", ""), super.plugin)
  }

  trait DuplicateRemoval extends Pluginable {
    private def removeDuplicates(text: String): String = {
      def _removeDuplicates(chars: List[Char]) : String =  chars match {
        case Nil => ""
        case char :: rest =>
          val charHasDuplicates = text.count(_ == char) > 1
          val x = if (charHasDuplicates) "" else char
          x + _removeDuplicates(rest)
      }

      _removeDuplicates(text.toList)
    }

    override def plugin: String => Option[String] = plug(removeDuplicates, super.plugin)
  }

  trait Rotating extends Pluginable {
    override def plugin: String => Option[String] = plug(
      (text: String) => text.init + text.last,
      super.plugin
    )
  }

  trait Doubling extends Pluginable {
     //SIP Also could be like: super.plugin((text map (c => s"$c$c")).mkString)       
    def doubleEverySecondChar(chars: List[Char]): String = repeatEveryNCharMTimes(chars, 2, 2)

    override def plugin: String => Option[String] = plug(
      (text: String) => doubleEverySecondChar(text.toList),
      super.plugin
    )
  }

  trait Shortening extends Pluginable {
    def removeEverySecondCharacter(chars: List[Char]): String = removeEveryNthChar(chars, 2)

    override def plugin: String => Option[String] = plug(
      (text: String) => removeEverySecondCharacter(text.toList),
      super.plugin
    )
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
 */
 //SIP Solution is correct
  private def removeEveryNthChar(chars: List[Char], n: Int): String = {
    def walk(chars: List[Char], index: Int): String = chars match {
      case Nil => ""
      case char :: rest =>
        val shouldRemove = index % n == 0
        val partialRes = if (shouldRemove) "" else char
        partialRes + walk(rest, index + 1)
    }

    val initialIndex = 1
    walk(chars, initialIndex)
  }
}
