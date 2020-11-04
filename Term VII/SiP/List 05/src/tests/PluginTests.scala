import plugins._

package object PluginTests {
  def runTests(): Unit = {
    testReverting()
    testLowerCasing()
    testChaining()
    testSingleSpacing()
    testNoSpacing()
    testRemoveDuplicates()
    testRotate()
    testDoubling()
    testShortening()
  }

  def foo(): Unit = {
    val revert = new Pluginable with Reverting with SingleSpacing
    val text = "hello, world!"

    val reverted = revert.plugin(text)

    print("????", reverted.get)
  }

  def testReverting(): Unit = {
    shouldRevertNonNullString()
    shouldHandleRevertingNull()
  }

  def testLowerCasing(): Unit = {
    shouldLowerCaseText()
  }

  def testChaining(): Unit = {
    shouldLowerCaseAndReverse()
  }

  def testSingleSpacing(): Unit = {
    shouldRemoveDuplicateSpaces()
  }

  def testNoSpacing(): Unit = {
    shouldRemoveAllSpaces()
  }

  def testRemoveDuplicates(): Unit = {
    shouldRemoveDuplicateCharacters()
  }

  def testRotate(): Unit = {
    shouldRotateStringByOne()
  }

  def testDoubling(): Unit = {
    shouldDoubleEverySecondCharacter()
  }

  def testShortening(): Unit = {
    shouldRemoveEveryOtherCharacter()
  }

  def shouldRevertNonNullString(): Unit = {
    val revert = new Pluginable with Reverting
    val text = "hello, world!"

    val reverted = revert.plugin(text)

    assert(reverted.get == "!dlrow ,olleh")
  }

  def shouldHandleRevertingNull(): Unit = {
    val revert = new Pluginable with Reverting
    val text = null

    val reverted = revert.plugin(text)

    assert(reverted.isEmpty)
  }

  def shouldLowerCaseText(): Unit = {
    val lowerCase = new Pluginable with LowerCasing
    val text = "Hello, World!"

    val lowered = lowerCase.plugin(text)

    assert(lowered.get == "hello, world!")
  }

  def shouldLowerCaseAndReverse(): Unit = {
    val chain = new Pluginable with Reverting with LowerCasing
    val text = "Hello, World!"

    val transformed = chain.plugin(text)

    assert(transformed.get == "!dlrow ,olleh")
  }

  def shouldRemoveDuplicateSpaces(): Unit = {
    val singleSpacing = new Pluginable with SingleSpacing
    val text = "hello,     world       !   "

    val singleSpaced = singleSpacing.plugin(text)

    assert(singleSpaced.get == "hello, world ! ")
  }

  def shouldRemoveAllSpaces(): Unit = {
    val noSpacing = new Pluginable with NoSpacing
    val text = "hello,     world       !   "

    val withoutSpaces = noSpacing.plugin(text)

    assert(withoutSpaces.get == "hello,world!")
  }

  def shouldRemoveDuplicateCharacters(): Unit = {
    val removeDuplicates = new Pluginable with DuplicateRemoval
    val text = "alzaa  cda"

    val noDuplicates = removeDuplicates.plugin(text)

    assert(noDuplicates.get == "lzcd")
  }

  def shouldRotateStringByOne(): Unit = {
    val rotate = new Pluginable with Rotating
    val text = "abc"

    val rotated = rotate.plugin(text)

    assert(rotated.get == "cab")
  }

  def shouldDoubleEverySecondCharacter(): Unit = {
    val double = new Pluginable with Doubling
    val text = "abcd"

    val doubled = double.plugin(text)

    assert(doubled.get == "abbcdd")
  }

  def shouldRemoveEveryOtherCharacter(): Unit = {
    val shorten = new Pluginable with Shortening
    val text = "abcd"

    val shortened = shorten.plugin(text)

    assert(shortened.get == "ac")
  }
}
