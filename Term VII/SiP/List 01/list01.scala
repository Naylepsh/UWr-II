import scala.collection.mutable.ListBuffer

//scalar product of two vectors xs and ys
def scalarUgly(xs: List[Int], ys: List[Int]) = {
  var product = 0
  var i = 0

  while (i < xs.length && i < ys.length) {
    var z = xs(i) * ys(i)
    product += z
    i += 1
  }

  product
}

def scalar(xs: List[Int], ys: List[Int]): Int = {
  ((for ((x,y) <- (xs zip ys)) yield x*y)).sum
}

// iterative scalar even tho List makes it inefficient
// def scalar(xs: List[Int], ys: List[Int]) = {
//   import scala.math.{min}
//   val length = min(xs.length, ys.length)
//   val products = for { i <- 0 to length - 1 } yield xs(i) * ys(i) 

//   products.toList.sum
// }


//quicksort algorithm
def sortUgly(xs: List[Int]): List[Int] = {
  val buf = new ListBuffer[Int]()
  buf ++= xs

  val stack = new scala.collection.mutable.Stack[Int] 
  var begin = 0
  var end = xs.length - 1
  stack.push(begin)
  stack.push(end)

  while (stack.length > 0) {
    end = stack.pop()
    begin = stack.pop()

    val pivotIndex = partitionUgly(buf, begin, end)

    if (pivotIndex - 1 > begin) {
      stack.push(begin)
      stack.push(pivotIndex - 1)
    }

    if (pivotIndex + 1 < end) {
      stack.push(pivotIndex + 1)
      stack.push(end)
    }
  }

  buf.toList
}

def partitionUgly(xs: ListBuffer[Int], begin: Int, end: Int) = {
  var pivot = xs(end)
  var i = begin - 1
  
  var j = begin
  while (j <= end - 1) {
    if (xs(j) <= pivot) {
      i += 1
      swapUgly(xs, i, j)
    }

    j += 1
  }
  swapUgly(xs, i+1, end)

  i + 1
}

def swapUgly(xs: ListBuffer[Int], i: Int, j: Int) = {
  var temp = xs(i)
  xs(i) = xs(j)
  xs(j) = temp
}

// since list.filter(cond) if technically equivalent to for (x <- xs; if cond), I'm gonna use that
//SIP good point
def sort(xs: List[Int]): List[Int] = {
  if (xs.length <= 1) return xs

  val pivot = xs(xs.length / 2)
  val lesserThanPivot = xs.filter(_ < pivot)
  val equalToPivot = xs.filter(_ == pivot)
  val greaterThanPivot = xs.filter(_ > pivot)

  sort(lesserThanPivot) ++ equalToPivot ++ sort(greaterThanPivot)
}


//checks if n is prime
def isPrimeUgly(n: Int): Boolean = {
  if (n == 0) return false

  var abs = n.abs
  if (abs == 1) return false

  var divisor = 2
  while (divisor * divisor <= abs) {
    if (n % divisor == 0) return false
    divisor += 1
  }

  true
}

def isPrime(n: Int): Boolean = {
  if (n == 0) return false

  val abs = n.abs
  if (abs == 1) return false

  for (divisor <- 2 to abs-1; if n % divisor == 0) yield { 
    return false 
  }

  true
}


//for given positive integer n, find all pairs of integers i and j, where 1 ≤ j < i < n such that i + j is prime
def primePairsUgly(n : Int): List[(Int, Int)] = {
  var pairs = new ListBuffer[(Int, Int)]()

  var i = 1
  while (i < n) {
    var j = 1
    while (j < i) {
      if (isPrime(i + j)) {
        pairs += ((i, j))
      }

      j += 1
    }

    i += 1
  }

  pairs.toList
}

def primePairs(n : Int): List[(Int, Int)] = {
  val pairs = for (
    i <- 1 to n - 1; 
    j <- 1 to i - 1; 
    if isPrime(i+j)) 
  yield ((i, j))

  pairs.toList
}


//create a list with all lines from given file
val filesHere = new java.io.File(".").listFiles
// completely pointless usage of while since fileBuf.getLines().toList gives us the same result.
// I couldn't find any 'reasonable' code that would use while loop for reading file content tho.
// (unless reading char by char with buf.hasNext is considered reasonable, but that's kinda... yikes)
def fileLinesUgly(file: java.io.File): List[String] = {
  var fileBuf = scala.io.Source.fromFile(file)
  var lines: List[String] = List();
  try {
    lines = fileBuf.getLines().toList
  } catch {
    case _: Throwable => lines = List()
  } finally {
    fileBuf.close
  }

  var stringBuf = new ListBuffer[String]()
  var lineCounter = 0
  while (lineCounter < lines.length) {
    stringBuf += lines(lineCounter)
    lineCounter += 1
  }

  stringBuf.toList
}

def fileLines(file: java.io.File): List[String] = {
  val buf = scala.io.Source.fromFile(file)
  try {
    val lines = buf.getLines().toList
    return lines
  } catch {
    case _: Throwable => return List()
  } finally {
    buf.close
  }
}


//print names of all .scala files which are in filesHere & are non empty
def printNonEmptyUgly(pattern: String): Unit = {
  var fileCounter = 0
  while (fileCounter < filesHere.length) {
    val file = filesHere(fileCounter)
    if (file.getName contains pattern) {
      var lineCounter = 0
      val lines = fileLines(file)
      while (lineCounter < lines.length) {
        val line = lines(lineCounter)
        if (line.length > 0) {
          println(line)
        }

        lineCounter += 1
      }
    }

    fileCounter += 1
  }
}

def printNonEmpty(pattern: String): Unit = {
  for (file <- filesHere; if file.getName contains pattern) {
    for (line <- fileLines(file); if line.length > 0) {
      println(line)
    }
  }
}