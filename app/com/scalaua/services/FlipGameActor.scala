package com.scalaua.services

import java.time.Instant

import akka.actor.{ActorLogging, ActorRef}
import akka.persistence.{PersistentActor, RecoveryCompleted}

case class ClientSession(ref: ActorRef, session: String)

class FlipGameActor() extends PersistentActor with ActorLogging {

  val props = FlipActorProps("playerA")

  private val rng: Rng = Rng.real

  var client: Option[ClientSession] = None
  var state: FlipState = FlipState.initial

  def sendPending(state: FlipState) = ???

  override def receiveRecover: Receive = {
    case evt: FlipEvent =>
      updateState(evt)
    case RecoveryCompleted =>
      //sendPending(state)
      log.info(s"Flip actor $persistenceId have completed recovery")
  }

  override def receiveCommand: Receive = {
    case Attach(session, _) =>
      client = Some(ClientSession(sender(), session))
      client.get.ref ! Attached(Instant.now())

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