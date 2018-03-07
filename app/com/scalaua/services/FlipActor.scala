package com.scalaua.services

import java.time.Instant
import javax.inject.Inject

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.persistence.{PersistentActor, RecoveryCompleted, SnapshotOffer}
import com.google.inject.assistedinject.Assisted

object FlipActor {

  trait Factory {
    def apply(props: FlipActorProps, @Assisted("walletActor") walletClient: ActorRef): Actor
  }

}

class FlipActor @Inject()(@Assisted val props: FlipActorProps, @Assisted("walletActor") val walletClient: ActorRef)
  extends PersistentActor with ActorLogging {

  private val rng: Rng = Rng.real

  var gameClient: Option[ActorRef] = None
  var session: Option[String] = None
  var state: FlipState = FlipState.initial

  def sendPending(state: FlipState) = ???

  override def receiveRecover: Receive = {
    case evt: FlipEvent =>
      updateState(evt)
    case RecoveryCompleted =>
      sendPending(state)
      log.info(s"Slot actor ${persistenceId} have completed recovery")
  }

  override def receiveCommand: Receive = {
/*    case Connect(gp@GameParams(_, sessionKey, _, channel, _), wsConnectionActor) =>
      wsConnectionActor ! Connected(self, gameCtx, gp, state.client.asInitImpacts)
      gameClient = Some(wsConnectionActor)

      val request = BalanceRequest(
        gctx.player.currency,
        sessionKey,
        validateSessionKey = true,
        gctx.player.playerId,
        state.walletSource(channel)(gctx)
      )
      walletClient ! request

    case WalletResponseEnvelope(Right(r: BalanceResponse), _) =>
      gameClient.get ! ClientImpacts(List(UpdateBalance(r.balance)))*/

    case cmd: FlipCommand if state.handleCommand(session.get, rng, props, Instant.now()).isDefinedAt(cmd) =>
      log.debug(s"Processing $cmd command in state: $state")
      state.handleCommand(session.get, rng, props, Instant.now())(cmd) match {
        case error@Left(_) =>
          gameClient.get ! error
        case Right(evt) =>
          persistEvent(evt)
      }

    case unknown@_ =>
      log.warning(s"Unhandled command $unknown")
      //sender ! Left(Error(ERROR, GameErrorCodes.COMMAND_NOT_ALLOWED, s"Command is not allowed in current state", TimeUtils.now(), None))

  }

  private def persistEvent(event: FlipEvent): Unit = {
    //val envelope = SlotEventEnvelope(gctx.actorId, state.roundId, gctx.epoch, gctx.player.currency, event)
    persist(event)(updateState)
  }

/*  def updateState(extEvent: SlotEventEnvelope): Unit =
    updateState(extEvent.event)*/

  def updateState(event: FlipEvent): Unit = {
    log.debug(s"applying $event to state")

    val newState = state.handleEvent(props)(event)

    // walletClient ! newState.walletImpact

    if (recoveryFinished) {
      //if (newState.client.impacts.nonEmpty)
        //gameClient.get ! ClientImpacts(newState.client.impacts.reverse)
      sendPending(newState)
      /*
                sendPromotionProgress(event)
                memorizeGameClient(event)
                replyAndForgetGameClient(event, newState)
                replyForOpsEvent(event, newState, state)
                autoStartNewRound(newState)
          sendPending(newState)
                takeSnapshot(newState)
      */
    }

    state = newState
  }

  override def persistenceId: String = s"flip-${props.playerId}"
}