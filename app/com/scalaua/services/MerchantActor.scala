package com.scalaua.services

import java.time.Instant

import akka.actor.{Actor, ActorLogging}
import javax.inject.Inject

import scala.collection.mutable
import scala.util.Random

class MerchantActor @Inject()() extends Actor with ActorLogging {

  val rng = Random

  val initialBalance = 10
  val balances: mutable.Map[String, Int] = mutable.Map()
  val confirmations: mutable.Map[String, WalletConfirmation] = mutable.Map()


  override def receive: Receive = {
    case WalletBalanceRequest(playerId, _) =>
      balances.getOrElseUpdate(playerId, initialBalance)
      sender() ! BalanceResponse(balances(playerId))

    case WalletRequest(id, requestType, amount, _, playerId, _) =>
      requestType match {
        case "BET" =>
          confirmations.get(id) match {
            case Some(confirmation) =>
              tryToSend(confirmation, playerId)
            case None =>
              val b = balances(playerId) - amount
              if (b >= 0) {
                balances(playerId) = b
                val confirmation = WalletConfirmation(id, amount, b, Instant.now())
                confirmations(id) = confirmation
                tryToSend(confirmation, playerId)
              } else {
                sender() ! WalletError4xx("error.not-enough-credits")
              }
          }

        case "WIN" =>
          confirmations.get(id) match {
            case Some(confirmation) =>
              tryToSend(confirmation, playerId)
            case None =>
              val b = balances(playerId) + amount
              balances(playerId) = b
              val confirmation = WalletConfirmation(id, amount, b, Instant.now())
              confirmations(id) = confirmation
              tryToSend(confirmation, playerId)
          }
      }
  }

  private def tryToSend(confirmation: WalletConfirmation, playerId: String): Unit = {
    if (failure(playerId)) {
      sender() ! WalletFailure5xx("failure.unexpected")
    } else {
      sender() ! confirmation
    }
  }

  def failure(playerId: String): Boolean = {
    val noiseLevel: Int = playerId match {
      case "AAA" => 0
      case "BBB" => 10
      case "CCC" => 20
      case "DDD" => 30
      case "EEE" => 40
      case "FFF" => 50
      case "GGG" => 60
      case "HHH" => 70
      case "III" => 80
      case _ => 15
    }
    noiseLevel > rng.nextInt(100)
  }

  override def unhandled(message: Any): Unit =
    log.error(s"unhandled wallet request $message")
}
