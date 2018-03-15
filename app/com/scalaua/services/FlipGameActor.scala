package com.scalaua.services

import java.time.Instant
import javax.inject.Inject

import akka.actor.{ActorLogging, ActorRef}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import com.google.inject.name.Named

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

class FlipGameActor @Inject()(@Named("merchant-actor") walletRef: ActorRef)
  extends PersistentActor with ActorLogging {

  val props = FlipActorProps("playerA")

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
    case Attach(session) =>
      persistEvent(Attached(session, Instant.now()))

    case Detach() =>
      persistEvent(Detached(Instant.now()))

    case br: BalanceResponse =>
      clientRef.get ! br

    case cmd: FlipCommand if state.handleCommand(rng, props, Instant.now()).isDefinedAt(cmd) =>
      log.debug(s"processing $cmd command in state: $state")
      state.handleCommand(rng, props, Instant.now())(cmd) match {
        case Left(error) =>
          clientRef.get ! error
        case Right(evt) =>
          persistEvent(evt)
      }

    case unexpected@_ =>
      log.warning(s"unexpected command $unexpected")
      clientRef.get ! FlipError("error.unexpected.command")

  }

  private def persistEvent(event: FlipEvent): Unit = {
    persist(event)(updateState)
  }

  def updateState(event: FlipEvent): Unit = {
    log.debug(s"applying $event to state")

    state = event match {
      case Attached(session, _) => state.attach(session)
      case Detached(_) => state.detach()
      case evt => state.handleEvent(props)(evt)
    }

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
        events.foreach(clientRef.get ! _)
        walletRef ! WalletBalanceRequest()
      case d: Detached =>
        clientRef.get ! d
        clientRef = None
      case e =>
        clientRef.get ! e
    }
  }

  private def sendToWallet(pendingRequest: Option[PendingRequest]): Unit = {
    implicit val ec: ExecutionContextExecutor = context.dispatcher

    pendingRequest match {
      case Some(p) if !p.undelivered && p.nrOfAttempts < 10 =>
        val redeliveryInterval = 30.seconds
        context.system.scheduler.scheduleOnce(redeliveryInterval * p.nrOfAttempts, () => walletRef ! p.walletRequest)
        ()
      case _ =>
        ()
    }
  }

}