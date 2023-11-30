package mlscript
package dsp

import typings.vscodeDebugadapter.mod.DebugSession
import typings.vscodeDebugprotocol.anon.{BreakpointsArray, Threads}
import typings.vscodeDebugprotocol.mod.DebugProtocol.{
  Breakpoint,
  Capabilities,
  ConfigurationDoneArguments,
  ConfigurationDoneResponse,
  InitializeRequestArguments,
  InitializeResponse,
  LaunchRequestArguments,
  LaunchResponse,
  Request,
  ScopesArguments,
  ScopesResponse,
  SetBreakpointsArguments,
  SetBreakpointsResponse,
  StackTraceArguments,
  StackTraceResponse,
  Thread,
  ThreadsResponse,
  VariablesArguments,
  VariablesResponse
}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.{Failure, Success}
import typings.vscodeDebugprotocol.mod.DebugProtocol.ContinueArguments
import typings.vscodeDebugprotocol.mod.DebugProtocol.ContinueResponse

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

  override def configurationDoneRequest(
      response: ConfigurationDoneResponse,
      args: ConfigurationDoneArguments,
      request: Request
  ): Unit =
    sendResponse(response)

  override def launchRequest(response: LaunchResponse, args: LaunchRequestArguments, request: Request): Unit =
    val program = request.arguments.get.asInstanceOf[js.Dynamic].selectDynamic("program").toString
    reporter.output(s"MLscript DSP server launched for $program")
    sendResponse(response)

    runtime.launch(program).andThen {
      case Success(_)     => reporter.output("MLscript DSP server successfully started")
      case Failure(error) => reporter.output(s"Error launching DSP server: $error")
    }

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

  override def continueRequest(response: ContinueResponse, args: ContinueArguments, request: Request): Unit =
    sendResponse(response)
    runtime.continue(args.threadId.toInt)
