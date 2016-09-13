// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim

import org.nlogo.agent.Patch
import org.nlogo.api.{ AgentException, LogoException}
import org.nlogo.core.Syntax
import org.nlogo.nvm.{ Context, EngineException, Reporter }
import org.nlogo.core.{ AgentKind, Reference, Referenceable }

class _patchvariable(private[this] val _vn: Int) extends Reporter with Referenceable {

  override def toString: String =
    s"${super.toString}:${if (world == null) vn else world.patchesOwnNameAt(vn)}"

  def makeReference: Reference = return new Reference(AgentKind.Patch, vn, this)

  override def report(context: Context) = report_1(context)

  def report_1(context: Context): AnyRef =
    try {
      context.agent.getPatchVariable(_vn)
    } catch {
      case ex: AgentException => throw new EngineException(context, this, ex.getMessage)
    }

  def vn = _vn
}
