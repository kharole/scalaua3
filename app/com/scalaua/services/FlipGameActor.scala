package com.scalaua.services

import java.time.Instant
import javax.inject.Inject

import akka.actor.{ActorLogging, ActorRef}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import com.google.inject.name.Named
import scala.concurrent.duration._

case class ClientSession(ref: ActorRef, session: String)

class FlipGameActor @Inject()(@Named("merchant-actor") walletRef: ActorRef)
  extends PersistentActor with ActorLogging {

  val props = FlipActorProps("playerA")

  private val rng: Rng = Rng.real

  var client: Option[ClientSession] = None
  var state: FlipState = FlipState.initial

  def sendPending(pendingRequest: Option[PendingRequest]): Unit = {
    implicit val ec = context.dispatcher

    pendingRequest match {
      case Some(p) if !p.undelivered && p.nrOfAttempts < 10 =>
        context.system.scheduler.scheduleOnce(30 seconds, () => walletRef ! p.walletRequest)
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
      log.info(s"Flip actor $persistenceId have completed recovery")
  }

  override def receiveCommand: Receive = {
    case Attach(session, _) =>
      client = Some(ClientSession(sender(), session))
      client.get.ref ! Attached(Instant.now())
      walletRef ! WalletBalanceRequest()
      log.info("attached")

    case br: BalanceResponse =>
      client.get.ref ! br

    case cmd: FlipCommand if state.handleCommand(client.get.session, rng, props, Instant.now()).isDefinedAt(cmd) =>
      log.debug(s"Processing $cmd command in state: $state")
      state.handleCommand(client.get.session, rng, props, Instant.now())(cmd) match {
        case error@Left(_) =>
          client.get.ref ! error
        case Right(evt) =>
          persistEvent(evt)
      }

    case unknown@_ =>
      log.warning(s"Unhandled command $unknown")
    //sender ! Left(Error(ERROR, GameErrorCodes.COMMAND_NOT_ALLOWED, s"Command is not allowed in current state", TimeUtils.now(), None))

  }

  private def persistEvent(event: FlipEvent): Unit = {
    persist(event)(updateState)
  }

  def updateState(event: FlipEvent): Unit = {
    log.debug(s"applying $event to state")
    val newState = state.handleEvent(props)(event)

    if (recoveryFinished) {
      client.get.ref ! event
      sendPending(newState.pendingRequest)
    }

    state = newState
  }

  override def persistenceId: String = s"flip-${props.playerId}"
}