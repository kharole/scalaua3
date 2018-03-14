package com.scalaua.web

import javax.inject._

import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Materializer
import com.google.inject.name.Named
import com.scalaua.services.FlipWsActor
import play.api.libs.streams.ActorFlow
import play.api.mvc.WebSocket.MessageFlowTransformer
import play.api.mvc._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class FlipController @Inject()(cc: ControllerComponents,
                               @Named("player-manager-actor") pma: ActorRef)
                              (implicit system: ActorSystem, materializer: Materializer) extends InjectedController {

  /**
    * Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  implicit val inFormat = WsInbound.format
  implicit val outFormat = WsOutbound.format
  implicit val messageFlowTransformer = MessageFlowTransformer.jsonMessageFlowTransformer[WsInbound, WsOutbound]

  def ws: WebSocket = WebSocket.accept[WsInbound, WsOutbound] { rq =>
    ActorFlow.actorRef(out => FlipWsActor.props(out, pma))
  }

}
