package controllers

import java.util.concurrent.{ArrayBlockingQueue, Callable}
import java.util.function.Consumer

import org.awaitility.Awaitility._
import com.scalaua.services.{Attach, BalanceUpdated, FlipCoin, WsOutbound}
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
          webSocket.sendMessage(Json.toJson(Attach("AAA")).toString())

          await().until(conditionNonEmpty)
          val rs0 = Json.parse(queue.take()).as[List[WsOutbound]]
          rs0.map(_.name) mustBe List("attached")

/*          await().until(conditionNonEmpty)
          val rs1 = Json.parse(queue.take()).as[List[WsOutbound]]
          rs1 mustBe List(BalanceUpdated(0))

          webSocket.sendMessage(Json.toJson(FlipCoin(100, "head")).toString())

          await().until(conditionNonEmpty)
          val rs2 = Json.parse(queue.take()).as[List[WsOutbound]]
          rs2.map(_.name) mustBe List("bet-accepted")*/
        }

      }

    }
  }
}
