package org.brianmckenna.wartremover
package test

import org.scalatest.FunSuite

import org.brianmckenna.wartremover.warts.NoNeedForMonad

class NoNeedForMonadTest extends FunSuite {
  test("Report cases where Applicative is enough") {
    val withWarnings = WartTestTraverser(NoNeedForMonad) {
      for {
        x <- List(1, 2, 3)
        y <- List(2, 3, 4)
      } yield x * y

      Option(1).flatMap(i => Option(2).map(j => i + j))
    }
    val noWarnings = WartTestTraverser(NoNeedForMonad) {
      for {
        x <- List(1,2,3)
        y <- x to 3
      } yield x * y

      Option(1).flatMap(i => Option(i + 1).map(j => i + j))
    }

    expectResult(List.empty, "result.errors")(withWarnings.errors)
    expectResult(List(NoNeedForMonad.message, NoNeedForMonad.message), "result.warnings")(withWarnings.warnings)

    expectResult(List.empty, "result.errors")(noWarnings.errors)
    expectResult(List.empty, "result.warnings")(noWarnings.warnings)
  }

  test("Work properly with function literals, eta-expanded functions, objects with apply methods") {
    val etaExpanded = WartTestTraverser(NoNeedForMonad) {
      def fun(in: Int) = 14
      val xs = for {
        y <- Nil
        x <- Option(3) map fun
      } yield x

      Option(3).flatMap { case t => Some(t) }
    }

    val extendsFunction = WartTestTraverser(NoNeedForMonad) {
      object test extends Function1[Int, Option[Int]] {
        def apply(i: Int) = Option(i + 2)
      }
      object test2 {
        def apply(i: Int) = Option(i + 4)
      }

      for {
        x <- Option(1)
        res <- test(x)
      } yield res
      for {
        x <- Option(2)
        res <- test2(x)
      } yield res
    }


    expectResult(List.empty, "result.errors")(etaExpanded.errors)
    expectResult(List(NoNeedForMonad.message), "result.warnings")(etaExpanded.warnings)

    expectResult(List.empty, "result.errors")(extendsFunction.errors)
    expectResult(List.empty, "result.errors")(extendsFunction.warnings)
  }
}
