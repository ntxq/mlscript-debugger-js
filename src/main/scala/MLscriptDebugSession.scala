import scala.scalajs.js
import typings.vscodeDebugprotocol.mod.DebugProtocol.{OutputEvent as _, *}
import typings.vscodeDebugadapter.mod.*

class MLscriptDebugSession extends LoggingDebugSession():
  def output(message: String, category: String = "console"): Unit =
    sendEvent(OutputEvent(message, category).asInstanceOf)

  override def initializeRequest(response: InitializeResponse, args: InitializeRequestArguments): Unit =
    val capabilities = Capabilities()
    capabilities.setSupportsSingleThreadExecutionRequests(true)

    response.body_InitializeResponse = capabilities
    sendResponse(response)
    sendEvent(InitializedEvent().asInstanceOf)

    output("MLscript debugger initialized\n")

