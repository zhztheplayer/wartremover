package org.wartremover
package warts

/**
 * Forbid a class from inheriting from a case class.
 */
object InheritFromCaseClass extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      override def traverse(tree: u.universe.Tree): Unit = {
        def checkClass(c: u.universe.ClassSymbol): Unit = {
          val caseBases = c.baseClasses
            .filter(_.isClass)
            .filter(base => base != c)
            .filter(base => base.asClass.isCaseClass)
          if (caseBases.nonEmpty) {
            error(u)(
              tree.pos,
              s"case class is not inheritable: ${c.name} is trying to inherit $caseBases")
          }
        }

        import u.universe._

        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case d: ModuleDef =>
            val m = d.symbol.asModule.moduleClass.asClass
            checkClass(m)
          case d: ClassDef =>
            val c = d.symbol.asClass
            checkClass(c)
          case t => super.traverse(tree)
        }
      }
    }
  }
}
