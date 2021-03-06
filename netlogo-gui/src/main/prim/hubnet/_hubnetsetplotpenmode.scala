// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.hubnet

import org.nlogo.api.{ CommandRunnable}
import org.nlogo.core.Syntax
import org.nlogo.core.PlotPenInterface
import org.nlogo.nvm.{ Command, Context, EngineException }

class _hubnetsetplotpenmode extends Command {

  override def perform(context: Context) {
    val name = argEvalString(context, 0)
    val mode = argEvalIntValue(context, 1)
    workspace.waitFor(
      new CommandRunnable {
        override def run() {
          if (mode < PlotPenInterface.MinMode || mode > PlotPenInterface.MaxMode)
            throw new EngineException(
              context, _hubnetsetplotpenmode.this,
              mode + " is not a valid plot pen mode (valid modes are 0, 1, and 2)")
          workspace.getHubNetManager.foreach(_.setPlotPenMode(name, mode))
        }})
    context.ip = next
  }
}
