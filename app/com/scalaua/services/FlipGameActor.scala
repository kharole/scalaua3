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

  def sendPending(pendingRequest: Option[PendingRequest]): Unit = {
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

  override def receiveRecover: Receive = {
    case evt: FlipEvent =>
      updateState(evt)
    case RecoveryCompleted =>
      sendPending(state.pendingRequest)
      log.info(s"flip actor $persistenceId have completed recovery")
  }

  override def receiveCommand: Receive = {
    case Attach(session) =>
      persistEvent(Attached(session, Instant.now()))
      clientRef = Some(sender())
      walletRef ! WalletBalanceRequest()

    case Detach() =>
      persistEvent(Detached(Instant.now()))
      //clientRef = None
      
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
    val newState = event match {
      case Attached(session, _) => state.attach(session)
      case Detached(_) => state.detach()
      case evt => state.handleEvent(props)(evt)
    }

    if (recoveryFinished) {
      clientRef.get ! event
      sendPending(newState.pendingRequest)
    }

    state = newState
  }

  override def persistenceId: String = s"flip-${props.playerId}"
}