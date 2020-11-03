import plugins._

import scala.annotation.tailrec

package object actions {
  // plugin applying plugins with order: SingleSpacing => Doubling => Shortening
  val actionA: Pluginable = new Pluginable with Shortening with Doubling with SingleSpacing

  // plugin applying plugins with order: NoSpacing => Shortening => Doubling
  val actionB: Pluginable = new Pluginable with Doubling with Shortening with NoSpacing

  // plugin applying plugins with order: LowerCasing => Doubling
  val actionC: Pluginable = new Pluginable with Doubling with LowerCasing

  // plugin applying plugins with order: DuplicateRemoval => Rotating
  val actionD: Pluginable = new Pluginable with Rotating with DuplicateRemoval

  // plugin applying plugins with order: NoSpacing => Shortening => Doubling => Reverting
  val actionE: Pluginable = new Pluginable with Reverting with Doubling with Shortening with NoSpacing

  // plugin applying plugin Rotating 5-times
  val actionF: Pluginable = new Pluginable {
    override def plug(text: String): Option[String] = repeatPlugin(new Pluginable with Rotating)(text, 5)
  }

  // plugin applying plugins with order: actionA => actionB
  val actionG: Pluginable = new Pluginable {
    override def plug(text: String): Option[String] = actionA.plug(text) match {
      case Some(str) =>
        actionB.plug(str)
      case None => None
    }
  }

  private def repeatPlugin(plugin: Pluginable): (String, Int) => Option[String] = {
    @tailrec
    def repeat(text: String, n: Int): Option[String] = n match {
      case 0 => Option(text)
      case _ =>
        plugin.plug(text) match {
          case Some(transformed) => repeat(transformed,  n - 1)
          case None => None
        }
    }

    repeat
  }
}
