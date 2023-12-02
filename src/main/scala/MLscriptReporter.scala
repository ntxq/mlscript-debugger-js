package mlscript
package dsp

import typings.vscodeDebugadapter.mod.{
  ExitedEvent,
  InitializedEvent,
  OutputEvent,
  StoppedEvent,
  TerminatedEvent,
  ThreadEvent
}
import typings.vscodeDebugprotocol.mod.DebugProtocol.Event

import scala.scalajs.js

class MLscriptReporter(sendEvent: Event => Unit):
  // These are same types but different namespaces
  given Conversion[
    typings.vscodeDebugadapter.libMessagesMod.Event,
    typings.vscodeDebugprotocol.mod.DebugProtocol.Event
  ] = _.asInstanceOf

  def initialized(): Unit =
    sendEvent(InitializedEvent())

  def output(message: String, category: String = "console"): Unit =
    sendEvent(OutputEvent(message + "\n", category))

  def thread(reason: "started" | "exited", threadId: Int): Unit =
    sendEvent(ThreadEvent(reason, threadId))

  def stop(breakpointId: Double, threadId: Int): Unit =
    val event = StoppedEvent("breakpoint", threadId.toDouble)
    event.body_StoppedEvent.setHitBreakpointIds(js.Array(breakpointId))
    event.body_StoppedEvent.setAllThreadsStopped(true)
    sendEvent(event)

  def exit(exitCode: Int): Unit =
    sendEvent(ExitedEvent(exitCode))

  def terminate(): Unit =
    sendEvent(TerminatedEvent(false))
