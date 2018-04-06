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

  override def unhandled(message: Any): Unit = {
    log.error(s"unhandled object $message in detached web socket")
  }

  def receive: Receive = detached

  def detached: Receive = {
    case wsa: WsAttach =>
      managerRef ! toFlipCommand(wsa)
    case a: Attached =>
      context.become(attached(sender()))
      toWsOutbounds(a).foreach(out ! _)

      //todo: add detach handling
  }

  def attached(gameRef: ActorRef): Receive = {
    case wsi: WsInbound =>
      gameRef ! toFlipCommand(wsi)

    case evt: FlipEvent =>
      toWsOutbounds(evt).foreach(out ! _)

    case BalanceResponse(balance) =>
      out ! WsBalanceUpdated(balance)

    case FlipError(msg) =>
      out ! WsShowDisposableMessage(msg)

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
      case BetAccepted(amount, alternative, _) =>
        List(WsBetAccepted(amount, alternative), WsStatusUpdated("collecting"))
      case BetConfirmed(confirmation, result, outcome, win, _) =>
        List(WsHideBlockingMessage(), WsFlipped(result, outcome, win), WsBalanceUpdated(confirmation.newBalance), WsStatusUpdated("paying-out"))
      case BetError(WalletError4xx(msg), _) =>
        List(WsShowDisposableMessage(msg))
      case BetAttemptFailed(_, _) =>
        List(WsShowBlockingMessage("your balance is temporary unavailable"))
      case WinConfirmed(confirmation, _) =>
        List(WsHideBlockingMessage(), WsBalanceUpdated(confirmation.newBalance), WsStatusUpdated("finished"))
      case WinError(_, _) =>
        List(WsShowBlockingMessage("please contact support"))
      case WinAttemptFailed(_, _) =>
        List(WsShowBlockingMessage("your balance is temporary unavailable"))
      case NewRoundStarted(roundId, _) =>
        List(WsNewRoundStarted(roundId), WsStatusUpdated("awaiting"))
      case Attached(_, _) =>
        List(WsAttached())
      case Detached(_) =>
        List(WsDetached())
    }
  }
}
