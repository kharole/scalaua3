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
      out ! List(WsOutbound("attached"))
  }

  def attached(gameRef: ActorRef): Receive = {
    case wsi: WsInbound =>
      gameRef ! wsi

    case wso: WsOutbound =>
      out ! wso

    case uo@_ =>
      log.error(s"unknown object $uo in web socket")

  }

}
