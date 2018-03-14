package com.scalaua.web

import play.api.libs.json._

sealed trait WsInbound {
  val name: String
}

case class WsAttach(session: String, name: String = "attach") extends WsInbound

case class WsDetach(name: String = "detach") extends WsInbound

case class WsFlipCoin(bet: Int, alternative: String, name: String = "flip-coin") extends WsInbound

case class WsStartNewRound(name: String = "start-new-round") extends WsInbound

sealed trait WsOutbound {
  val name: String
}

case class WsBalanceUpdated(balance: Int, name: String = "balance-updated") extends WsOutbound

case class WsBetAccepted(bet: Int, alternative: String, name: String = "bet-accepted") extends WsOutbound

case class WsAttached(name: String = "attached") extends WsOutbound

case class WsDetached(name: String = "detached") extends WsOutbound

case class WsFlipped(result: String, outcome: String, win: Int, name: String = "flipped") extends WsOutbound

case class WsStatusUpdated(status: String, name: String = "status-updated") extends WsOutbound

case class WsNewRoundStarted(round: Int, name: String = "new-round-started") extends WsOutbound

case class WsShowDisposableMessage(message: String, name: String = "show-disposable-message") extends WsOutbound

case class WsShowBlockingMessage(message: String, name: String = "show-blocking-message") extends WsOutbound

case class WsHideBlockingMessage(message: String, name: String = "hide-disposable-message") extends WsOutbound

//json

object WsAttach {
  implicit val format: OFormat[WsAttach] = Json.format[WsAttach]
}

object WsDetach {
  implicit val format: OFormat[WsDetach] = Json.format[WsDetach]
}

object WsFlipCoin {
  implicit val format: OFormat[WsFlipCoin] = Json.format[WsFlipCoin]
}

object WsStartNewRound {
  implicit val format: OFormat[WsStartNewRound] = Json.format[WsStartNewRound]
}

object WsInbound {
  val w: OWrites[WsInbound] = {
    case c: WsAttach => WsAttach.format.writes(c)
    case c: WsDetach => WsDetach.format.writes(c)
    case c: WsFlipCoin => WsFlipCoin.format.writes(c)
    case c: WsStartNewRound => WsStartNewRound.format.writes(c)
  }
  val r: Reads[WsInbound] = (json: JsValue) => {
    (json \ "name").as[String] match {
      case "attach" => WsAttach.format.reads(json)
      case "detach" => WsDetach.format.reads(json)
      case "flip-coin" => WsFlipCoin.format.reads(json)
      case "start-new-round" => WsStartNewRound.format.reads(json)
    }
  }
  implicit val format: OFormat[WsInbound] = OFormat(r, w)
}

object WsAttached {
  implicit val format: OFormat[WsAttached] = Json.format[WsAttached]
}

object WsDetached {
  implicit val format: OFormat[WsDetached] = Json.format[WsDetached]
}

object WsBalanceUpdated {
  implicit val format: OFormat[WsBalanceUpdated] = Json.format[WsBalanceUpdated]
}

object WsBetAccepted {
  implicit val format: OFormat[WsBetAccepted] = Json.format[WsBetAccepted]
}

object WsFlipped {
  implicit val format: OFormat[WsFlipped] = Json.format[WsFlipped]

}

object WsStatusUpdated {
  implicit val format: OFormat[WsStatusUpdated] = Json.format[WsStatusUpdated]

}

object WsNewRoundStarted {
  implicit val format: OFormat[WsNewRoundStarted] = Json.format[WsNewRoundStarted]
}


object WsOutbound {
  val w: OWrites[WsOutbound] = {
    case wso: WsAttached => WsAttached.format.writes(wso)
    case wso: WsDetached => WsDetached.format.writes(wso)
    case wso: WsBalanceUpdated => WsBalanceUpdated.format.writes(wso)
    case wso: WsBetAccepted => WsBetAccepted.format.writes(wso)
    case wso: WsFlipped => WsFlipped.format.writes(wso)
    case wso: WsStatusUpdated => WsStatusUpdated.format.writes(wso)
    case wso: WsNewRoundStarted => WsNewRoundStarted.format.writes(wso)

  }
  val r: Reads[WsOutbound] = (json: JsValue) => {
    (json \ "name").as[String] match {
      case "attached" => WsAttached.format.reads(json)
      case "detached" => WsDetached.format.reads(json)
      case "balance-updated" => WsBalanceUpdated.format.reads(json)
      case "bet-accepted" => WsBetAccepted.format.reads(json)
      case "flipped" => WsFlipped.format.reads(json)
      case "status-updated" => WsStatusUpdated.format.reads(json)
      case "new-round-started" => WsNewRoundStarted.format.reads(json)
    }
  }
  implicit val format: OFormat[WsOutbound] = OFormat(r, w)
}
