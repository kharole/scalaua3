package controllers

import java.util.concurrent.{ArrayBlockingQueue, Callable}
import java.util.function.Consumer

import org.awaitility.Awaitility._
import com.scalaua.web._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test._
import play.shaded.ahc.org.asynchttpclient.AsyncHttpClient

import scala.compat.java8.FutureConverters
import scala.concurrent.duration._

class FlipControllerSpec extends PlaySpec with ScalaFutures {

  //TODO: send new round on attach
  //TODO: send accumulated events on attach
  //TODO: send status-updated
  //TODO: send start-new-round on validation error
  //TODO: send show(ok) msg on validation error
  //TODO: send show/hide msg on wallet delays

  "FlipController" should {
    "attach and flip" in WsTestClient.withClient { client =>
      lazy val port: Int = Helpers.testServerPort
      val app = new GuiceApplicationBuilder().build()
      Helpers.running(TestServer(port, app)) {
        val myPublicAddress = s"localhost:$port"
        val serverURL = s"ws://$myPublicAddress/ws"

        val asyncHttpClient: AsyncHttpClient = client.underlying[AsyncHttpClient]
        val webSocketClient = new WebSocketClient(asyncHttpClient)
        val queue = new ArrayBlockingQueue[String](10)
        val origin = serverURL
        val consumer: Consumer[String] = (message: String) => queue.put(message)
        val listener = new WebSocketClient.LoggingListener(consumer)
        val completionStage = webSocketClient.call(serverURL, origin, listener)
        val f = FutureConverters.toScala(completionStage)

        // Test we can get good output from the websocket
        whenReady(f, timeout = Timeout(1.second)) { webSocket =>
          val conditionOpen: Callable[java.lang.Boolean] = () => webSocket.isOpen
          val conditionNonEmpty: Callable[java.lang.Boolean] = () => queue.peek() != null

          await().until(conditionOpen)
          webSocket.sendMessage(Json.toJson(WsAttach("AAA")).toString())

          await().until(conditionNonEmpty)
          val rs0 = Json.parse(queue.take()).as[WsOutbound]
          rs0.name mustBe "attached"

          await().until(conditionNonEmpty)
          val rs1 = Json.parse(queue.take()).as[WsOutbound]
          rs1 mustBe WsBalanceUpdated(0)

          webSocket.sendMessage(Json.toJson(WsFlipCoin(5, "head")).toString())

          await().until(conditionNonEmpty)
          val rs2 = Json.parse(queue.take()).as[WsOutbound]
          rs2.name mustBe "bet-accepted"

          await().until(conditionNonEmpty)
          val rs3 = Json.parse(queue.take()).as[WsOutbound]
          rs3.name mustBe "flipped"

          await().until(conditionOpen)
          webSocket.sendMessage(Json.toJson(WsStartNewRound()).toString())

          await().until(conditionNonEmpty)
          val rs4 = Json.parse(queue.take()).as[WsOutbound]
          rs4.name mustBe "new-round-started"
        }

      }

    }
  }
}
