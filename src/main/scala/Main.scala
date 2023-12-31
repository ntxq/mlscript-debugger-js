package mlscript
package dsp

import typings.vscode.{mod as vscode}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object Main:
  @JSExportTopLevel("activate")
  def activate(context: vscode.ExtensionContext): Unit =
    registerHelloWorld(context)
    registerRunFile(context)
    registerDebugger(context)

  def registerHelloWorld(context: vscode.ExtensionContext): Unit =
    val helloWorldFn: Any => Any     = _ => vscode.window.showInformationMessage("Hello World!")
    val jsFn: js.Function1[Any, Any] = helloWorldFn

    val disposable = vscode.commands.registerCommand(
      "extension.mlscript.helloworld",
      jsFn
    )
    context.subscriptions.push(disposable.asInstanceOf)

  def registerRunFile(context: vscode.ExtensionContext): Unit =
    val debugFn: Any => Any = _ =>
      for editor <- vscode.window.activeTextEditor
      yield vscode.debug.startDebugging(
        (),
        js.Dynamic
          .literal(
            "name"    -> "Run File",
            "type"    -> "mlscript",
            "request" -> "launch",
            "program" -> editor.document.uri.fsPath
          )
          .asInstanceOf[vscode.DebugConfiguration]
      )
    val jsFn: js.Function1[Any, Any] = debugFn

    val disposable = vscode.commands.registerCommand(
      "extension.mlscript.runEditorContents",
      jsFn
    )
    context.subscriptions.push(disposable.asInstanceOf)

  def registerDebugger(context: vscode.ExtensionContext): Unit =
    val factory: vscode.DebugAdapterDescriptorFactory = MLscriptDebugAdapterDescriptorFactory().asInstanceOf
    val disposable = vscode.debug.registerDebugAdapterDescriptorFactory("mlscript", factory)
    context.subscriptions.push(disposable.asInstanceOf)
