package com.scalaua.modules

import com.google.inject.AbstractModule
import com.scalaua.services.{FlipGameActor, ManagerActor, MerchantActor}
import play.api.libs.concurrent.AkkaGuiceSupport

class AkkaModule extends AbstractModule with AkkaGuiceSupport {
  override def configure(): Unit = {
    bindActor[ManagerActor]("manager-actor")
    bindActor[MerchantActor]("merchant-actor")
    bindActorFactory[FlipGameActor, FlipGameActor.Factory]
  }
}
