package controllers

import com.scalaua.web._
import org.scalatestplus.play._

class FlipControllerSpec extends PlaySpec with WsSpec {
  
  "FlipController" should {
    "attach flip and detach" in {
      sendMessage(WsAttach("AAA"))
      receiveMessage.name mustBe "attached"
      receiveMessage.name mustBe "new-round-started"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "balance-updated"
      sendMessage(WsFlipCoin(5, "head"))
      receiveMessage.name mustBe "bet-accepted"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "hide-blocking-message"
      receiveMessage.name mustBe "balance-updated"
      receiveMessage.name mustBe "flipped"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "hide-blocking-message"
      receiveMessage.name mustBe "balance-updated"
      receiveMessage.name mustBe "status-updated"
      sendMessage(WsStartNewRound())
      receiveMessage.name mustBe "new-round-started"
      receiveMessage.name mustBe "status-updated"
      sendMessage(WsDetach())
      receiveMessage.name mustBe "detached"
    }

    "recover round state" in {
      sendMessage(WsAttach("AAA"))
      receiveMessage.name mustBe "attached"
      receiveMessage.name mustBe "new-round-started"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "balance-updated"
      sendMessage(WsFlipCoin(5, "head"))
      receiveMessage.name mustBe "bet-accepted"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "hide-blocking-message"
      receiveMessage.name mustBe "balance-updated"
      receiveMessage.name mustBe "flipped"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "hide-blocking-message"
      receiveMessage.name mustBe "balance-updated"
      receiveMessage.name mustBe "status-updated"
      sendMessage(WsDetach())
      receiveMessage.name mustBe "detached"
      
      sendMessage(WsAttach("AAA"))
      receiveMessage.name mustBe "attached"
      receiveMessage.name mustBe "new-round-started"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "bet-accepted"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "hide-blocking-message"
      receiveMessage.name mustBe "balance-updated"
      receiveMessage.name mustBe "flipped"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "hide-blocking-message"
      receiveMessage.name mustBe "balance-updated"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "balance-updated"
      sendMessage(WsStartNewRound())
      receiveMessage.name mustBe "new-round-started"
      receiveMessage.name mustBe "status-updated"
      sendMessage(WsDetach())
      receiveMessage.name mustBe "detached"
    }

    "show disposable error" in {
      sendMessage(WsAttach("AAA"))
      receiveMessage.name mustBe "attached"
      receiveMessage.name mustBe "new-round-started"
      receiveMessage.name mustBe "status-updated"
      receiveMessage.name mustBe "balance-updated"
      sendMessage(WsFlipCoin(-5, "head"))
      receiveMessage mustBe WsShowDisposableMessage("invalid bet value")
      sendMessage(WsDetach())
      receiveMessage.name mustBe "detached"
    }
  }
}
