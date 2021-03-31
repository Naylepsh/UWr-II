import scala.annotation.tailrec

object Utils {
  @tailrec
  def isSorted(as: List[Int], ordering: (Int, Int) => Boolean): Boolean = as match {
     //SIP could be also:
    // as match { 
    //   case _ :: Nil | Nil => true
    //   case a :: b :: rest  => ordering(a, b) && isSorted(b :: rest, ordering)

    case Nil | _ => true
    case x :: xs => ordering(x, xs.head) && isSorted(xs, ordering)
  }

  def isAscSorted(as: List[Int]) = {
    isSorted(as, _ < _)
  }

  def isDescSorted(as: List[Int]) = {
    isSorted(as, _ > _)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def walk(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case y :: ys => walk(ys, f(acc, y))
    }

    walk(l, z)
  }

  def sum(l: List[Int])  = {
    foldLeft(l, 0)(_+_)
  }

  def length[A](l: List[A]) = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def compose[A, B, C](f: B => C, g: A => B)  = {
    x: A => f(g(x))
  }

//  I don't really think the [A, B] type makes much sense here,
//  as stacking f on top of each other requires their input type to be equal to output type,
//  so I changed it to [A]
//SIP right
  def repeated[A](f: A => A, n: Int): A => A = {
    require(n > 0)
    if (n == 1) {
      f
    } else {
      (x: A) => f(repeated(f, n - 1)(x))
    }
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
}
