/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest
import java.math.BigInteger

object BigIntegerTest extends JasmineTest {

  describe("java.math.BigInteger") {

    it("should provide `compareTo`") {
      def compare(x: String, y: String): Int =
        new BigInteger(x).compareTo(new BigInteger(y))

      expect(compare("0", "5")).toBeLessThan(0)
      expect(compare("10", "9")).toBeGreaterThan(0)
      expect(compare("-2", "-1")).toBeLessThan(0)
      expect(compare("3", "3")).toEqual(0)
    }

    it("should parse strings in base 16") {
      def test(s: String, v: String): Unit = {
        expect(new BigInteger(s, 16).compareTo(new BigInteger(v))).toEqual(0)
      }

      test("0", "0")
      test("5", "5")
      test("ff", "255")
      test("-24", "-36")
      test("30000", "196608")
      test("-90000", "-589824")
    }
  }
}
