// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.hubnet;

import org.nlogo.api.LogoException;
import org.nlogo.core.Syntax;
import org.nlogo.nvm.Context;
import org.nlogo.nvm.EngineException;

public final strictfp class _hubnetmakeplotnarrowcast
    extends org.nlogo.nvm.Command {


  @Override
  public void perform(final Context context)
      throws LogoException {
    final String name = argEvalString(context, 0);

    workspace.waitFor
        (new org.nlogo.api.CommandRunnable() {
          public void run() throws LogoException {
            if (!workspace.getHubNetManager().get().addNarrowcastPlot(name)) {
              throw new EngineException
                  (context, _hubnetmakeplotnarrowcast.this,
                      "no such plot: \"" + name + "\"");
            }
          }
        });
    context.ip = next;
  }
}
