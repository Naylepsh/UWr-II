package cards

import org.scalatest.funspec.AnyFunSpec

class CardsTest extends AnyFunSpec {
  describe("Numerical card creation") {
    it("should fail when given less than 2 pips") {
      val pips = 1
      assertThrows[IllegalArgumentException](Numerical(pips))
    }

    it("should fail when given more than 11 pips") {
      val pips = 11
      assertThrows[IllegalArgumentException](Numerical(pips))
    }
  }
}
