// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.core.Syntax
import org.nlogo.nvm.{ Context, Reporter }

class _patchsize extends Reporter {

  override def report(context: Context) =
    Double.box(report_1(context))
  def report_1(context: Context) =
    world.patchSize
}
