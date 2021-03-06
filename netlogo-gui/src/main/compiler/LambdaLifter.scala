// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.compiler

import org.nlogo.{ core, nvm, prim => coreprim },
  core.Let

import scala.collection.immutable.Stack

/**
 * Removes the bodies of command lambdas and makes them into separate "child" procedures.
 */

class LambdaLifter(lambdaNumbers: Iterator[Int]) extends DefaultAstVisitor {
  val children = collection.mutable.Buffer[ProcedureDefinition]()
  private var procedures = Stack.empty[nvm.Procedure]

  private var procedure = Option.empty[nvm.Procedure]
  override def visitProcedureDefinition(procdef: ProcedureDefinition) {
    procedure = Some(procdef.procedure)
    procedures = procedures.push(procdef.procedure)
    super.visitProcedureDefinition(procdef)
    procedures = procedures.pop
  }

  override def visitReporterApp(expr: ReporterApp) {
    expr.reporter match {
      case c: coreprim._commandlambda =>
        for (p <- procedure) {
          val formals = c.argumentNames.map(n => Let(n)).toArray
          val name = "__lambda-" + lambdaNumbers.next()
          val newProc =
            new nvm.Procedure(false, c.token, name, None, parent = procedures.head, lambdaFormals = formals)
          c.proc     = newProc
          c.proc.pos = expr.start
          c.proc.end = expr.end
          p.children += c.proc

          children +=
            new ProcedureDefinition(c.proc, expr.args(0).asInstanceOf[CommandBlock].statements)

          procedures = procedures.push(newProc)
          super.visitReporterApp(expr)
          procedures = procedures.pop
          expr.removeArgument(0)
        }
      case _ =>
        super.visitReporterApp(expr)
    }
  }
}
