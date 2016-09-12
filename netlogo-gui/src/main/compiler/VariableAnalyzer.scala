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

import org.nlogo.core.prim.{ _ask, _askconcurrent, _let, _repeat }

object VariableAnalyzer {
  sealed trait VariableConstraint
  case object ProcedureVariable extends VariableConstraint
  case object LetVariable       extends VariableConstraint
  case class AnonProcedure(constraint: VariableConstraint) extends VariableConstraint
  case class AskBlock(constraint: VariableConstraint)      extends VariableConstraint
  case class LoopBlock(constraint: VariableConstraint)     extends VariableConstraint

  type ConstraintMap = Map[String, VariableConstraint]

  def apply(proc: CoreProcedureDefinition): Map[String, VariableConstraint] = {
    val analyzer = new VariableAnalyzer()
    analyzer.visitProcedureDefinition(proc)(Map())
  }
}

import VariableAnalyzer._

class VariableAnalyzer extends AstFolder[Map[String, VariableConstraint]] {
  override def visitProcedureDefinition(proc: CoreProcedureDefinition)(a: ConstraintMap): ConstraintMap = {
    val newmap = proc.procedure.args.map(_ -> ProcedureVariable).toMap
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
    app.args.foldLeft(a) { case (acc, arg) => visitExpression(arg)(acc) }

  override def visitReporterBlock(block: CoreReporterBlock)(implicit a: ConstraintMap): ConstraintMap =
    visitReporterApp(block.app)

  override def visitStatement(stmt: CoreStatement)(implicit a: ConstraintMap): ConstraintMap = {
    stmt.command match {
      case _ask() | _askconcurrent() => a ++ enclosedConstraints(super.visitStatement(stmt)(_), AskBlock)
      case  _repeat()                => a ++ enclosedConstraints(super.visitStatement(stmt)(_), LoopBlock)
      case _let(Some(Let(name)))     => super.visitStatement(stmt)(a + (name.toUpperCase -> LetVariable))
      case _                         => super.visitStatement(stmt)
    }
  }

  private def enclosedConstraints(
    superCall: ConstraintMap => ConstraintMap,
    wrapConstraint: VariableConstraint => VariableConstraint): ConstraintMap = {
      superCall(Map()).map {
        case (k, v) => k -> wrapConstraint(v)
      }.toMap
  }
}
