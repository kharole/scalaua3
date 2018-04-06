package com.scalaua.services

import javax.inject.Inject

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.google.inject.name.Named
import play.api.libs.concurrent.InjectedActorSupport

import scala.collection.mutable

class ManagerActor @Inject()(factory: FlipGameActor.Factory, @Named("merchant-actor") walletRef: ActorRef) extends Actor
  with ActorLogging
  with InjectedActorSupport {

  val playerToGame: mutable.Map[String, ActorRef] = mutable.Map()

  override def preStart(): Unit = {
    log.info("manager actor start")
  }

  override def postStop(): Unit = {
    log.info("manager actor stop")
  }

  override def receive: Receive = {
    case attach@Attach(session) =>
      val playerId = session.split("-")(0)
      playerActor(playerId) forward attach
  }

  def playerActor(playerId: String): ActorRef = {
    playerToGame.getOrElseUpdate(playerId,
      injectedChild(factory(FlipActorProps(playerId), walletRef), s"$playerId-flip-actor"))
  }

}
