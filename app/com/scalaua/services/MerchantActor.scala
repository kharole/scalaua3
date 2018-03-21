package com.scalaua.services

import java.time.Instant
import javax.inject.Inject

import akka.actor.{Actor, ActorLogging}

import scala.collection.mutable

class MerchantActor @Inject()() extends Actor with ActorLogging {

  val initialBalance = 10
  val balances: mutable.Map[String, Int] = mutable.Map()

  override def receive: Receive = {
    case WalletBalanceRequest(playerId, _) =>
      balances.getOrElseUpdate(playerId, initialBalance)
      sender() ! BalanceResponse(balances(playerId))

    case WalletRequest(id, requestType, amount, ts, playerId, session) =>
      requestType match {
        case "BET" =>
          val b = balances(playerId) - amount
          if (b >= 0) {
            balances(playerId) = b
            sender() ! WalletConfirmation(id, amount, b, Instant.now())
          } else {
            sender() ! WalletError4xx("error.not-enough-credits")
          }
        case "WIN" =>
          val b = balances(playerId) + amount
          balances(playerId) = b
          sender() ! WalletConfirmation(id, amount, b, Instant.now())
        case _ =>
          log.error("unexpected wallet request")
      }
  }

}
