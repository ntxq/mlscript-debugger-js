package mlscript
package dsp

import typings.vscodeDebugadapter.mod.DebugSession
import typings.vscodeDebugprotocol.anon.BreakpointsArray
import typings.vscodeDebugprotocol.anon.Threads
import typings.vscodeDebugprotocol.mod.DebugProtocol.Breakpoint
import typings.vscodeDebugprotocol.mod.DebugProtocol.Capabilities
import typings.vscodeDebugprotocol.mod.DebugProtocol.ConfigurationDoneArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.ConfigurationDoneResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.InitializeRequestArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.InitializeResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.LaunchRequestArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.LaunchResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.Request
import typings.vscodeDebugprotocol.mod.DebugProtocol.ScopesArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.ScopesResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.SetBreakpointsArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.SetBreakpointsResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.StackTraceArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.StackTraceResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.Thread
import typings.vscodeDebugprotocol.mod.DebugProtocol.ThreadsResponse
import typings.vscodeDebugprotocol.mod.DebugProtocol.VariablesArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.VariablesResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

class MLscriptDebugSession extends DebugSession():
  val reporter                 = MLscriptReporter(sendEvent)
  val runtime: MLscriptRuntime = MLscriptRuntime(reporter)

  override def initializeRequest(response: InitializeResponse, args: InitializeRequestArguments): Unit =
    val capabilities = Capabilities()
    capabilities.setSupportsSingleThreadExecutionRequests(true)
    response.setBody(capabilities)
    sendResponse(response)
    reporter.initialized()

    reporter.output("MLscript debugger initialized")

  override def setBreakPointsRequest(
      response: SetBreakpointsResponse,
      args: SetBreakpointsArguments,
      request: Request
  ): Unit =
    val bps = (for
      breakpoints <- args.breakpoints
      source = args.source
    yield runtime.setBreakpoints(breakpoints.toSeq, source))
      .getOrElse(BreakpointsArray(js.Array()))
    response.setBody(bps)
    sendResponse(response)

  override def launchRequest(response: LaunchResponse, args: LaunchRequestArguments, request: Request): Unit =
    val program = request.arguments.get.asInstanceOf[js.Dynamic].selectDynamic("program").toString
    reporter.output(s"MLscript DSP server launched for $program")
    sendResponse(response)

    runtime.launch(program).andThen {
      case Success(_)     => reporter.output("MLscript DSP server successfully started")
      case Failure(error) => reporter.output(s"Error launching DSP server: $error")
    }

  override def configurationDoneRequest(
      response: ConfigurationDoneResponse,
      args: ConfigurationDoneArguments,
      request: Request
  ): Unit =
    sendResponse(response)

  override def threadsRequest(response: ThreadsResponse, request: Request): Unit =
    response.setBody(Threads(js.Array(Thread(runtime.threadId.toDouble, "MLscript Main Thread"))))
    sendResponse(response)

  override def stackTraceRequest(response: StackTraceResponse, args: StackTraceArguments, request: Request): Unit =
    response.setBody(runtime.getStackFrames)
    sendResponse(response)

  override def scopesRequest(response: ScopesResponse, args: ScopesArguments, request: Request): Unit =
    response.setBody(runtime.getScopes)
    sendResponse(response)

  override def variablesRequest(response: VariablesResponse, args: VariablesArguments, request: Request): Unit =
    response.setBody(runtime.getVariables)
    sendResponse(response)
