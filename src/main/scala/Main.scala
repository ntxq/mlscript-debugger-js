import scala.scalajs.js
import scala.scalajs.js.annotation.*
import typings.vscode.{mod as vscode}

object Main:
  @JSExportTopLevel("activate")
  def activate(context: vscode.ExtensionContext): Unit =
    registerHelloWorld(context)
    registerDebugger(context)

  /** Register a command that prints "Hello World" to the console (for testing purposes) */
  def registerHelloWorld(context: vscode.ExtensionContext): Unit =
    val helloWorldFn: Any => Any     = _ => vscode.window.showInformationMessage("Hello World!")
    val jsFn: js.Function1[Any, Any] = helloWorldFn

    val disposable = vscode.commands.registerCommand(
      "extension.mlscript.helloworld",
      jsFn
    )
    context.subscriptions.push(disposable.asInstanceOf)

  def registerDebugger(context: vscode.ExtensionContext): Unit =
    val debugFn: Any => Any = _ =>
      for editor <- vscode.window.activeTextEditor
      yield vscode.window.showInformationMessage(s"Running ${editor.document.uri}")
    val jsFn: js.Function1[Any, Any] = debugFn

    val disposable = vscode.commands.registerCommand(
      "extension.mlscript.runEditorContents",
      jsFn
    )
    context.subscriptions.push(disposable.asInstanceOf)
