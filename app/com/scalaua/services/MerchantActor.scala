package com.scalaua.services

import java.time.Instant
import javax.inject.Inject

import akka.actor.{Actor, ActorLogging}

class MerchantActor @Inject()() extends Actor with ActorLogging {

  override def receive: Receive = {
    case WalletBalanceRequest() => sender() ! BalanceResponse(0)

    case WalletRequest(id, requestType, amount, ts) => WalletConfirmation(id, amount, 23, Instant.now())
  }

}
