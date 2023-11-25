package mlscript
package dsp

import scala.scalajs.js
import typings.vscode.{mod as vscode}

class MLscriptDebugAdapterDescriptorFactory extends js.Object:
  def createDebugAdapterDescriptor(session: vscode.DebugSession): vscode.ProviderResult[vscode.DebugAdapterDescriptor] =
    vscode.DebugAdapterInlineImplementation(MLscriptDebugSession().asInstanceOf)
