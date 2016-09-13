// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.compiler

import org.nlogo.core.{
  AstFolder,
  Let,
  ProcedureDefinition => CoreProcedureDefinition,
  CommandBlock => CoreCommandBlock,
  Expression => CoreExpression,
  ReporterApp => CoreReporterApp,
  ReporterBlock => CoreReporterBlock,
  Statement => CoreStatement,
  Statements => CoreStatements
}

import org.nlogo.core.prim.{ _ask, _askconcurrent, _commandlambda, _let, _letvariable,
  _procedurevariable, _repeat, _reporterlambda, _set }

object VariableAnalyzer {
  sealed trait VariableConstraint
  case object ProcedureVariable extends VariableConstraint
  case object LetVariable       extends VariableConstraint
  case class AnonProcedure(constraint: VariableConstraint) extends VariableConstraint
  case class AskBlock(constraint: VariableConstraint)      extends VariableConstraint
  case class LoopBlock(constraint: VariableConstraint)     extends VariableConstraint

  type ConstraintMap = Map[String, Seq[VariableConstraint]]

  def apply(proc: CoreProcedureDefinition): ConstraintMap = {
    val analyzer = new VariableAnalyzer()
    analyzer.visitProcedureDefinition(proc)(Map())
  }
}

import VariableAnalyzer._

class VariableAnalyzer extends AstFolder[ConstraintMap] {
  override def visitProcedureDefinition(proc: CoreProcedureDefinition)(a: ConstraintMap): ConstraintMap = {
    val newmap = proc.procedure.args.map(_ -> Seq(ProcedureVariable)).toMap
    super.visitProcedureDefinition(proc)(a ++ newmap)
  }

  override def visitCommandBlock(block: CoreCommandBlock)(implicit a: ConstraintMap): ConstraintMap =
    visitStatements(block.statements)

  override def visitExpression(exp: CoreExpression)(implicit a: ConstraintMap): ConstraintMap =
    exp match {
      case app: CoreReporterApp  => visitReporterApp(app)
      case cb: CoreCommandBlock  => visitCommandBlock(cb)
      case rb: CoreReporterBlock => visitReporterBlock(rb)
    }

  override def visitReporterApp(app: CoreReporterApp)(implicit a: ConstraintMap): ConstraintMap =
    app.reporter match {
      case _: _commandlambda | _: _reporterlambda =>
        a ++ enclosedConstraints(super.visitReporterApp(app)(_), AnonProcedure)
      case _ => super.visitReporterApp(app)
    }

  override def visitReporterBlock(block: CoreReporterBlock)(implicit a: ConstraintMap): ConstraintMap =
    visitReporterApp(block.app)

  override def visitStatement(stmt: CoreStatement)(implicit a: ConstraintMap): ConstraintMap = {
    stmt.command match {
      case _ask() | _askconcurrent() => merge(a, enclosedConstraints(super.visitStatement(stmt)(_), AskBlock))
      case  _repeat()                => merge(a, enclosedConstraints(super.visitStatement(stmt)(_), LoopBlock))
      case _: _set                   =>
        stmt.args.head.asInstanceOf[CoreReporterApp].reporter match {
          case _letvariable(Let(name)) => super.visitStatement(stmt)(a + (name.toUpperCase -> Seq(LetVariable)))
          case _procedurevariable(_, name) => super.visitStatement(stmt)(a + (name.toUpperCase -> Seq(ProcedureVariable)))
          case _ => super.visitStatement(stmt)
        }
      case _let(Some(Let(name)))     => super.visitStatement(stmt)(a + (name.toUpperCase -> Seq(LetVariable)))
      case _                         => super.visitStatement(stmt)
    }
  }

  private def enclosedConstraints(
    superCall: ConstraintMap => ConstraintMap,
    wrapConstraint: VariableConstraint => VariableConstraint): ConstraintMap = {
      superCall(Map()).map {
        case (k, v) => k -> v.map(wrapConstraint)
      }.toMap
  }

  private def merge(a: ConstraintMap, b: ConstraintMap): ConstraintMap = {
    (a.keySet ++ b.keySet).map { k =>
      k -> (a.getOrElse(k, Seq()) ++ b.getOrElse(k, Seq()))
    }.toMap
  }
}
