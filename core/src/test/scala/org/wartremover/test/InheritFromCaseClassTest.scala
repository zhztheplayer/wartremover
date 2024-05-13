package org.wartremover
package test

import org.scalatest.funsuite.AnyFunSuite
import org.wartremover.warts.InheritFromCaseClass

class InheritFromCaseClassTest extends AnyFunSuite with ResultAssertions {
  test("class cannot extend case class") {
    val result = WartTestTraverser(InheritFromCaseClass) {
      case class Foo(i: Int)
      class Bar(i: Int) extends Foo(i)
    }
    assertError(result)("case class is not inheritable: Bar is trying to inherit List(class Foo)")
  }

  test("object cannot extend case class") {
    val result = WartTestTraverser(InheritFromCaseClass) {
      case class Foo(i: Int)
      object Bar extends Foo(0)
    }
    assertError(result)("case class is not inheritable: Bar is trying to inherit List(class Foo)")
  }

  test("InheritFromCaseClass wart obeys SuppressWarnings") {
    val result = WartTestTraverser(InheritFromCaseClass) {
      case class Foo(i: Int)
      @SuppressWarnings(Array("org.wartremover.warts.InheritFromCaseClass"))
      class Bar(i: Int) extends Foo(i)
    }
    assertEmpty(result)
  }
}
