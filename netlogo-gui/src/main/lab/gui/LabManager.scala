// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.lab.gui

import org.nlogo.api.LabProtocol
import org.nlogo.api.ModelSection
import org.nlogo.core.Model
import org.nlogo.window.{GUIWorkspace, EditDialogFactoryInterface, LabManagerInterface}
import org.nlogo.workspace.{CurrentModelOpener, WorkspaceFactory}
import org.nlogo.window.Events._
import scala.collection.mutable.ListBuffer

class LabManager(val workspace: GUIWorkspace,
                 dialogFactory: EditDialogFactoryInterface,
                 val workspaceFactory: WorkspaceFactory with CurrentModelOpener)
  extends LabManagerInterface
  with CompiledEvent.Handler
  with LoadBeginEvent.Handler
  with LoadModelEvent.Handler
{
  val protocols = new ListBuffer[LabProtocol]

  def getComponent: Seq[LabProtocol] = protocols.toSeq
  def defaultComponent: Seq[LabProtocol] = Seq()

  def addProtocol(p: LabProtocol): Unit = {
    protocols += p
  }

  def clearProtocols(): Unit = {
    protocols.clear()
  }
  private lazy val dialog = new ManagerDialog(this, dialogFactory)
  def show() { dialog.update(); dialog.setVisible(true) }
  def close() { dialog.setVisible(false) }
  def dirty() { new DirtyEvent().raise(this) }
  /// Event.LinkChild -- lets us get events out to rest of app
  val getLinkParent = workspace
  /// loading & saving
  def handle(e:LoadBeginEvent) {
    close()
    protocols.clear()
    lastCompileAllWasSuccessful = false
  }
  def handle(e:LoadModelEvent) {
    protocols ++= e.model
      .optionalSectionValue[Seq[LabProtocol]]("org.nlogo.modelsection.behaviorspace")
      .getOrElse(Seq[LabProtocol]())
  }
  override def updateModel(m: Model): Model =
    m.withOptionalSection("org.nlogo.modelsection.behaviorspace", Some(protocols), Seq())

  /// making sure everything gets compiled before an experiment run
  private var lastCompileAllWasSuccessful = false
  def handle(e:CompiledEvent) {
    if(e.sourceOwner.isInstanceOf[org.nlogo.window.ProceduresInterface])
      lastCompileAllWasSuccessful = e.error == null
  }
  def prepareForRun() {
    (new CompileAllEvent).raise(this)
    if(!lastCompileAllWasSuccessful)
      throw new org.nlogo.awt.UserCancelException
  }
}
