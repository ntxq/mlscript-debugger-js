package mlscript
package dsp

import typings.vscodeDebugadapter.libDebugSessionMod.InitializedEvent
import typings.vscodeDebugadapter.mod.DebugSession
import typings.vscodeDebugadapter.mod.OutputEvent
import typings.vscodeDebugadapter.mod.ThreadEvent
import typings.vscodeDebugprotocol.mod.DebugProtocol.Breakpoint
import typings.vscodeDebugprotocol.mod.DebugProtocol.Capabilities
import typings.vscodeDebugprotocol.mod.DebugProtocol.InitializeRequestArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.InitializeResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.InitializedEvent
import typings.vscodeDebugprotocol.mod.DebugProtocol.LaunchRequestArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.LaunchResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.Request
import typings.vscodeDebugprotocol.mod.DebugProtocol.SetBreakpointsArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.SetBreakpointsResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

class MLscriptDebugSession extends DebugSession():
  val runtime: MLscriptRuntime = MLscriptRuntime(output(_, "console"), output(_, "stdout"))

  def output(message: String, category: String = "console"): Unit =
    sendEvent(OutputEvent(message + "\n", category).asInstanceOf)

  def thread(reason: "started" | "exited", threadId: Int): Unit =
    sendEvent(ThreadEvent(reason, threadId).asInstanceOf)

  override def initializeRequest(response: InitializeResponse, args: InitializeRequestArguments): Unit =
    val capabilities = Capabilities()
    capabilities.setSupportsSingleThreadExecutionRequests(true)

    response.body_InitializeResponse = capabilities
    sendResponse(response)
    sendEvent(InitializedEvent().asInstanceOf)

    output("MLscript debugger initialized")

  override def setBreakPointsRequest(
      response: SetBreakpointsResponse,
      args: SetBreakpointsArguments,
      request: Request
  ): Unit =
    val bps = (for
      breakpoints <- args.breakpoints
      source = args.source
    yield runtime.setBreakpoints(breakpoints.toSeq, source).toArray).getOrElse(Array.empty[Breakpoint])

  override def launchRequest(response: LaunchResponse, args: LaunchRequestArguments, request: Request): Unit =
    val program = request.arguments.get.asInstanceOf[js.Dynamic].selectDynamic("program").toString
    output(s"MLscript DSP server launched for $program")
    sendResponse(response)

    runtime.launch(program).andThen {
      case Success(_)     => thread("started", runtime.threadId)
      case Failure(error) => output(s"Error launching DSP server: $error")
    }
