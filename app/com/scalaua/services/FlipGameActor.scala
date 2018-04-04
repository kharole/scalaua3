package com.scalaua.services

import java.time.Instant

import javax.inject.Inject
import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import com.google.inject.assistedinject.Assisted

import scala.collection.immutable
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

object FlipGameActor {

  trait Factory {
    def apply(props: FlipActorProps, walletRef: ActorRef): Actor
  }

}

class FlipGameActor @Inject()(@Assisted props: FlipActorProps, @Assisted walletRef: ActorRef)
  extends PersistentActor with ActorLogging {

  private val rng: Rng = Rng.real

  var clientRef: Option[ActorRef] = None
  var state: FlipState = FlipState.initial
  var events: List[FlipEvent] = List(NewRoundStarted(0, Instant.now()))

  override def persistenceId: String = s"flip-${props.playerId}"

  override def receiveRecover: Receive = {
    case evt: FlipEvent =>
      updateState(evt)
    case RecoveryCompleted =>
      sendToWallet(state.pendingRequest)
      log.info(s"flip actor $persistenceId have completed recovery")
  }

  override def receiveCommand: Receive = {
    case br: BalanceResponse if clientRef.nonEmpty =>
      clientRef.get ! br

    //todo: pf orElse
    case cmd: FlipCommand if state.behaviour.validateCommand(rng, props, Instant.now()).isDefinedAt(cmd) =>
      log.debug(s"processing $cmd command in state: $state")
      state.behaviour.validateCommand(rng, props, Instant.now())(cmd) match {
        case Left(error) =>
          clientRef.get ! error
        case Right(evts) =>
          persistEvents(evts)
      }
  }
  
  override def unhandled(message: Any): Unit = {
    log.warning(s"unhandled command $message in state $state")
    clientRef.get ! FlipError("unhandled command")
  }

  private def persistEvents(evts: immutable.Seq[FlipEvent]): Unit = {
    persistAll(evts)(updateState)
  }

  def updateState(event: FlipEvent): Unit = {
    log.debug(s"applying $event to state")

    state = state.behaviour.handleEvent(props)(event)

    events = event match {
      case Attached(_, _) | Detached(_) => events
      case nrs: NewRoundStarted => List(nrs)
      case evt => evt :: events
    }

    if (recoveryFinished) {
      sendToClient(event)
      sendToWallet(state.pendingRequest)
    }
  }

  //side effects
  private def sendToClient(event: FlipEvent): Unit = {
    event match {
      case a: Attached =>
        clientRef = Some(sender())
        clientRef.get ! a
        events.reverse.foreach(clientRef.get ! _)
        walletRef ! WalletBalanceRequest(props.playerId, state.session.get)
      case d: Detached =>
        clientRef.get ! d
        clientRef = None
      case e =>
        clientRef.get ! e
    }
  }

  private def sendToWallet(pendingRequest: Option[PendingRequest]): Unit = {
    implicit val ec: ExecutionContextExecutor = context.dispatcher

    //todo: timers
    pendingRequest match {
      case Some(p) if !p.undelivered && p.nrOfAttempts < 10 =>
        val redeliveryInterval = 10.seconds
        context.system.scheduler.scheduleOnce(redeliveryInterval * p.nrOfAttempts, () => walletRef ! p.walletRequest)
        ()
      case _ =>
        ()
    }
  }

}