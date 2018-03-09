package com.scalaua.modules

import com.google.inject.AbstractModule
import com.scalaua.services.{FlipGameActor, MerchantActor, PlayerManagerActor}
import play.api.libs.concurrent.AkkaGuiceSupport

class AkkaModule extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
    bindActor[PlayerManagerActor]("player-manager-actor")
    bindActor[FlipGameActor]("playerA-flip-actor")
    bindActor[MerchantActor]("merchant-actor")
  }
}
