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
    case _: Attached =>
      context.become(attached(sender()))
      out ! WsAttached()
  }

  def attached(gameRef: ActorRef): Receive = {
    case wsi: WsInbound =>
      gameRef ! toFlipCommand(wsi)

    case evt: FlipEvent =>
      toWsOutbounds(evt).foreach(out ! _)

    case BalanceResponse(balance) =>
      out ! WsBalanceUpdated(balance)

    case FlipError(code) =>
      WsShowDisposableMessage(code)

    case unexpected@_ =>
      log.error(s"unexpected object $unexpected in web socket")

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
      case BetsAccepted(_, amount, alternative, _) =>
        List(WsBetAccepted(amount, alternative))
      case BetsConfirmed(_, result, _) =>
        List(WsFlipped(result, "", 0))
      case BetError(_, _) =>
        List()
      case BetAttemptFailed(_, _) =>
        List()
      case WinConfirmed(_, _) =>
        List()
      case WinError(_, _) =>
        List()
      case WinAttemptFailed(_, _) =>
        List()
      case NewRoundStarted(_) =>
        List(WsNewRoundStarted(0))
      case Attached(_, _) =>
        List()
      case Detached(_) =>
        List()
    }
  }
}
