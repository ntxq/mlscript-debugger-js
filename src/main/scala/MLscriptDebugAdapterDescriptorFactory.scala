package mlscript
package dsp

import typings.vscode.{mod as vscode}

import scala.scalajs.js

class MLscriptDebugAdapterDescriptorFactory extends js.Object:
  def createDebugAdapterDescriptor(session: vscode.DebugSession): vscode.ProviderResult[vscode.DebugAdapterDescriptor] =
    vscode.DebugAdapterInlineImplementation(MLscriptDebugSession().asInstanceOf)
