package mlscript
package dsp

import scala.scalajs.js
import typings.vscodeDebugprotocol.mod.DebugProtocol.{OutputEvent as _, *}
import typings.vscodeDebugadapter.mod.*

class MLscriptDebugSession extends DebugSession():
  val runtime: MLscriptRuntime = MLscriptRuntime(output(_, "console"), output(_, "stdout"))

  def output(message: String, category: String = "console"): Unit =
    sendEvent(OutputEvent(message + "\n", category).asInstanceOf)

  override def initializeRequest(response: InitializeResponse, args: InitializeRequestArguments): Unit =
    val capabilities = Capabilities()
    capabilities.setSupportsSingleThreadExecutionRequests(true)

    response.body_InitializeResponse = capabilities
    sendResponse(response)
    sendEvent(InitializedEvent().asInstanceOf)

    output("MLscript debugger initialized")


  override def launchRequest(response: LaunchResponse, args: LaunchRequestArguments, request: Request): Unit = 
    val program = request.arguments.get.asInstanceOf[js.Dynamic].selectDynamic("program").toString
    output(s"MLscript DSP server launched for $program")
    sendResponse(response)

    runtime.launch(program)