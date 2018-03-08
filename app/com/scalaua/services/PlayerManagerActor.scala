package com.scalaua.services

import javax.inject.Inject

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.google.inject.name.Named

class PlayerManagerActor @Inject()(@Named("playerA-flip-actor") pa: ActorRef) extends Actor with ActorLogging {

  override def receive: Receive = {
    case any@_ => pa forward any
  }

}
