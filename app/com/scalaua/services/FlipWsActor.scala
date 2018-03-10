package com.scalaua.services

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

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
    case a: Attach =>
      managerRef ! a
    case a: Attached =>
      context.become(attached(sender()))
      out ! WsAttached()
  }

  def attached(gameRef: ActorRef): Receive = {
    case wsi: WsInbound =>
      gameRef ! wsi

    case BalanceResponse(balance) =>
      out ! WsBalanceUpdated(balance)

    case evt: FlipEvent => evt match {
      case BetsAccepted(session, amount, alternative, timestamp) =>
        out ! WsBetAccepted(amount, alternative)
      case BetsConfirmed(confirmation, result, timestamp) =>
        out ! WsFlipped(result, "", 0)
      case BetError(reason, timestamp) =>
        ???
      case BetAttemptFailed(reason, timestamp) =>
        ???
      case WinConfirmed(confirmation, timestamp) =>
        //out ! WsNewRoundStarted()
      case WinError(reason, timestamp) =>
        ???
      case WinAttemptFailed(reason, timestamp) =>
        ???
      case NewRoundStarted(timestamp) =>
        out ! WsNewRoundStarted(0)
      case Attached(timestamp) =>
        ???
      case Detached(timestamp) =>
        ???
    }

    case uo@_ =>
      log.error(s"unknown object $uo in web socket")

  }

}
