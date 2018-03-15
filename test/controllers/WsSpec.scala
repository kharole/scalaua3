package controllers

import java.util.concurrent.{ArrayBlockingQueue, Callable}
import java.util.function.Consumer

import com.scalaua.web.{WsInbound, WsOutbound}
import org.awaitility.Awaitility.await
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Args, Status, TestSuite}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsValue, Json}
import play.api.test.WsTestClient
import play.api.{Application, Mode}
import play.shaded.ahc.org.asynchttpclient.AsyncHttpClient
import play.shaded.ahc.org.asynchttpclient.ws.WebSocket

import scala.compat.java8.FutureConverters
import scala.concurrent.duration._
import scala.language.postfixOps

trait WsSpec extends GuiceOneServerPerSuite with ScalaFutures {
  this: TestSuite =>

  override def fakeApplication(): Application = new GuiceApplicationBuilder()
    .in(Mode.Test)
    .build()

  private lazy val queue = new ArrayBlockingQueue[String](10)
  private lazy val conditionWebSocketNonEmpty: Callable[java.lang.Boolean] = () => queue.peek() != null
  lazy val myPublicAddress = s"localhost:$port"
  lazy val serverURL = s"ws://$myPublicAddress/ws"
  private var webSocket: Option[WebSocket] = None

  private def socket: WebSocket = webSocket match {
    case Some(ws) => ws
    case None => throw new IllegalStateException("Web wSocket has been not initialized yet")
  }

  private def conditionSocketOpen: Callable[java.lang.Boolean] = () => socket.isOpen

  def sendMessage(msg: Any): Unit = {
    val message: String = msg match {
      case c: WsInbound => Json.toJson(c).toString
      case c: JsValue => c.toString
      case c: String => c
      case c@_ => throw new IllegalArgumentException(s"Can not send message:'$c'")
    }
    socket.sendMessage(message)
    ()
  }

  def receiveMessage: WsOutbound = {
    await().until(conditionWebSocketNonEmpty)
    takeServerMessages
  }

  private def takeRawMessages: String = queue.take()

  private def takeServerMessages: WsOutbound = Json.parse(takeRawMessages).as[WsOutbound]

  abstract protected override def runTest(testName: String, args: Args): Status = {
    WsTestClient.withClient { client =>
      val asyncHttpClient: AsyncHttpClient = client.underlying[AsyncHttpClient]
      val webSocketClient = new WebSocketClient(asyncHttpClient)
      val origin = serverURL
      val consumer: Consumer[String] = (message: String) => queue.put(message)
      val listener = new WebSocketClient.LoggingListener(consumer)
      val completionStage = webSocketClient.call(serverURL, origin, listener)
      val f = FutureConverters.toScala(completionStage)

      // Test we can get good output from the websocket
      whenReady(f, timeout = Timeout(1.second)) { ws: WebSocket =>
        webSocket = Some(ws)
        await().until(conditionSocketOpen)
        val newConfigMap = args.configMap + ("org.scalatestplus.web.webSocket" -> ws) + ("org.scalatestplus.web.webSocket.queue" -> queue)
        val newArgs = args.copy(configMap = newConfigMap)
        val status = super.runTest(testName, newArgs)
        status

      }
    }


  }
}