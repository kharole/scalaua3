package com.scalaua.services

import java.time.Instant

import akka.actor.{Actor, ActorLogging, ActorRef, Timers}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import com.google.inject.assistedinject.Assisted
import javax.inject.Inject

import scala.collection.immutable
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.math.signum

object FlipGameActor {

  trait Factory {
    def apply(props: FlipActorProps, walletRef: ActorRef): Actor
  }

}

class FlipGameActor @Inject()(@Assisted props: FlipActorProps, @Assisted walletRef: ActorRef)
  extends Timers with PersistentActor with ActorLogging {

  private val rng: Rng = Rng.real

  var clientRef: Option[ActorRef] = None
  var state: FlipState = FlipState.initial
  var events: List[FlipEvent] = List(NewRoundStarted(0, Instant.now()))

  override def preStart(): Unit = {
    log.info("game actor start")
  }

  override def postStop(): Unit = {
    log.info("game actor stop")
  }

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

    case wr: WalletRequest =>
      walletRef ! wr
      log.info(s"request $wr sent to wallet")

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
  }

  private def persistEvents(evts: immutable.Seq[FlipEvent]): Unit = {
    persistAll(evts)(updateState)
  }

  def updateState(event: FlipEvent): Unit = {
    log.info(s"applying $event to state")

    state = state.behaviour.handleEvent(props)(event)

    log.info(s"new state is $state")

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
    pendingRequest match {
      case Some(p) if p.nrOfAttempts < 10 =>
        val delay = 10.seconds * signum(p.nrOfAttempts)
        timers.startSingleTimer("wallet-timer", p.walletRequest, delay)
        log.info(s"wallet request ${p.walletRequest} scheduled in $delay")
        ()
      case _ =>
        ()
    }
  }

}