// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.agent.{ AgentSet, LinkManager, Turtle }
import org.nlogo.core.AgentKind
import org.nlogo.nvm.{ Context, EngineException, Reporter }

class _mylinks(val breedName: String) extends Reporter {

  def this() = this(null)

  override def toString =
    super.toString + ":" + breedName

  override def report(context: Context): AnyRef = {
    val breed =
      if (breedName == null) world.links
      else world.getLinkBreed(breedName)
    AgentSet.fromIterator(AgentKind.Link,
      world.linkManager.findLinksWith(context.agent.asInstanceOf[Turtle], breed))
  }
}
