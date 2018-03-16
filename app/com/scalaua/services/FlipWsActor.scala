package com.scalaua.services

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.scalaua.web._

object FlipWsActor {
  def props(out: ActorRef, managerRef: ActorRef) = Props(new FlipWsActor(out, managerRef))
}

class FlipWsActor(out: ActorRef, managerRef: ActorRef) extends Actor with ActorLogging {

  override def preStart(): Unit = {
    log.info("ws connected")
  }

  override def postStop(): Unit = {
    log.info("ws disconnected")
  }

  def receive: Receive = detached

  def detached: Receive = {
    case wsa: WsAttach =>
      managerRef ! toFlipCommand(wsa)
    case a: Attached =>
      context.become(attached(sender()))
      toWsOutbounds(a).foreach(out ! _)

    case unexpected@_ =>
      log.error(s"unexpected object $unexpected in detached web socket")
  }

  def attached(gameRef: ActorRef): Receive = {
    case wsi: WsInbound =>
      gameRef ! toFlipCommand(wsi)

    case evt: FlipEvent =>
      toWsOutbounds(evt).foreach(out ! _)

    case BalanceResponse(balance) =>
      out ! WsBalanceUpdated(balance)

    case FlipError(code) =>
      out ! WsShowDisposableMessage(code)

    case unexpected@_ =>
      log.error(s"unexpected object $unexpected in attached web socket")

  }

  private def toFlipCommand(wsi: WsInbound): FlipCommand = {
    wsi match {
      case WsAttach(session, _) =>
        Attach(session)
      case WsDetach(_) =>
        Detach()
      case WsFlipCoin(bet, alternative, _) =>
        FlipCoin(bet, alternative)
      case WsStartNewRound(_) =>
        StartNewRound()
    }
  }

  private def toWsOutbounds(evt: FlipEvent): List[WsOutbound] = {
    evt match {
      case BetsAccepted(amount, alternative, _) =>
        List(WsBetAccepted(amount, alternative))
      case BetsConfirmed(confirmation, result, outcome, win, _) =>
        List(WsFlipped(result, outcome, win), WsBalanceUpdated(confirmation.newBalance))
      case BetError(_, _) =>
        List()
      case BetAttemptFailed(_, _) =>
        List()
      case WinConfirmed(confirmation, _) =>
        List(WsBalanceUpdated(confirmation.newBalance))
      case WinError(_, _) =>
        List()
      case WinAttemptFailed(_, _) =>
        List()
      case NewRoundStarted(roundId, _) =>
        List(WsNewRoundStarted(roundId))
      case Attached(_, _) =>
        List(WsAttached())
      case Detached(_) =>
        List(WsDetached())
    }
  }
}
