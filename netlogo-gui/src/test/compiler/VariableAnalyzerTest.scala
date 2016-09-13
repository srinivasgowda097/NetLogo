// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.compiler

import org.scalatest.FunSuite

import org.nlogo.core.{ ProcedureDefinition => CoreProcedureDefinition }
import org.nlogo.parse.{ FrontEnd }

import VariableAnalyzer._

class VariableAnalyzerTest extends FunSuite {
  def compile(source: String): CoreProcedureDefinition = {
    val (coreDefs, _) = FrontEnd.frontEnd(source)
    coreDefs.head
  }

  def assertAnalysisDefines(code: String, varName: String, constraint: VariableConstraint): Unit = {
    assert(VariableAnalyzer(compile(code))(varName.toUpperCase).contains(constraint))
  }

  def assertAnalysisUndefined(code: String, varName: String): Unit = {
    assert(! VariableAnalyzer(compile(code)).isDefinedAt(varName.toUpperCase))
  }

  test("procedure variables are returned in the analysis") {
    assertAnalysisUndefined("to foo end", "bar")
    assertAnalysisDefines("to foo [bar] end", "bar", ProcedureVariable)
    assertAnalysisDefines("to foo [bar baz] end", "baz", ProcedureVariable)
    assertAnalysisUndefined("to foo [bar] end", "baz")
  }

  test("a procedure with a single let variable returns that as part of the analysis") {
    assertAnalysisDefines("to foo let bar 0 end", "bar", LetVariable)
  }

  test("a procedure with a let variable and a procedure variable returns both in analysis") {
    val code = "to foo [bar] let baz 0 end"
    assertAnalysisDefines(code, "bar", ProcedureVariable)
    assertAnalysisDefines(code, "baz", LetVariable)
  }

  test("a let-variable in an ask block is returned in analysis") {
    assertAnalysisDefines("to foo ask turtles [ let bar 0 ] end", "bar", AskBlock(LetVariable))
    assertAnalysisDefines("to foo ask-concurrent turtles [ let bar 0 ] end", "bar", AskBlock(LetVariable))
  }

  test("a let-variable in a task is returned by the analyzer") {
    assertAnalysisDefines("to foo run [ [] -> let bar 0 ] end", "bar", AnonProcedure(LetVariable))
  }

  test("a let-variable in a looped block is returned in analysis") {
    assertAnalysisDefines("to foo repeat 10 [ let bar 0 print bar ] end", "bar", LoopBlock(LetVariable))
  }

  test("a let-variable in a loop closed over by an anonymous procedure is returned in analysis") {
    assertAnalysisDefines("to foo repeat 10 [ let bar 0 run [ [] -> set bar 10 ] ] end", "bar", LoopBlock(AnonProcedure(LetVariable)))
  }
}
