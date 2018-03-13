package com.scalaua.services

import java.time.Instant

import play.api.libs.json._

import scala.util.{Random, Right}


object Rng {
  def fixed(value: Int): Rng = (n: Int) => value

  def real: Rng = new Rng {
    val r = Random

    override def next(n: Int): Int = r.nextInt(n)
  }
}

trait Rng {
  def next(n: Int): Int
}

//model

case class PendingRequest(walletRequest: WalletRequest, nrOfAttempts: Int = 0, undelivered: Boolean = false) {
  def incNrOfAttempts: PendingRequest = copy(nrOfAttempts = nrOfAttempts + 1)

  def resetNrOfAttempts: PendingRequest = copy(nrOfAttempts = 0)

  def undeliver: PendingRequest = copy(undelivered = true)

  def resetUndeliver: PendingRequest = copy(undelivered = false)

  def delayMultiplier: Long = if (nrOfAttempts > 0) pow2to(nrOfAttempts - 1) else 0L

  private def pow2to(n: Int): Long = 1L << n
}

case class WalletBalanceRequest()

case class WalletRequest(id: String, requestType: String, amount: Int, ts: Instant)

case class FlipError(code: String)

//commands

sealed trait FlipCommand

sealed trait WalletResponse extends FlipCommand

case class BalanceResponse(balance: Int)

case class WalletConfirmation(id: String, amount: Int, newBalance: Int, processedAt: Instant) extends WalletResponse

case class WalletError4xx(code: String) extends WalletResponse

case class WalletFailure5xx(code: String) extends WalletResponse

sealed trait WsInbound extends FlipCommand {
  val name: String
}

case class Attach(session: String, name: String = "attach") extends WsInbound

case class Detach(name: String = "detach") extends WsInbound

case class FlipCoin(bet: Int, alternative: String, name: String = "flip-coin") extends WsInbound

case class StartNewRound(name: String = "start-new-round") extends WsInbound

//events
trait Event {
  val timestamp: Instant
}

sealed trait FlipEvent extends Event {
  def newBalance: Option[Int] = this match {
    case rc: ConfirmationEvent =>
      Some(rc.confirmation.newBalance)
    case _ =>
      None
  }
}

trait FlipWalletError {
  val reason: WalletError4xx
}

trait FlipAttemptFailed {
  val reason: WalletFailure5xx
}


trait ConfirmationEvent {
  val confirmation: WalletConfirmation
}

case class BetsAccepted(session: String, amount: Int, alternative: String, timestamp: Instant) extends FlipEvent

case class BetsConfirmed(confirmation: WalletConfirmation, result: String, timestamp: Instant) extends FlipEvent with ConfirmationEvent

case class BetError(reason: WalletError4xx, timestamp: Instant) extends FlipEvent with FlipWalletError

case class BetAttemptFailed(reason: WalletFailure5xx, timestamp: Instant) extends FlipEvent with FlipAttemptFailed

case class WinConfirmed(confirmation: WalletConfirmation, timestamp: Instant) extends FlipEvent with ConfirmationEvent

case class WinError(reason: WalletError4xx, timestamp: Instant) extends FlipEvent with FlipWalletError

case class WinAttemptFailed(reason: WalletFailure5xx, timestamp: Instant) extends FlipEvent with FlipAttemptFailed

case class NewRoundStarted(timestamp: Instant) extends FlipEvent

case class Attached(timestamp: Instant) extends FlipEvent

case class Detached(timestamp: Instant) extends FlipEvent

sealed trait WsOutbound {
  val name: String
}

case class WsBalanceUpdated(balance: Int, name: String = "balance-updated") extends WsOutbound

case class WsBetAccepted(bet: Int, alternative: String, name: String = "bet-accepted") extends WsOutbound

case class WsAttached(name: String = "attached") extends WsOutbound

case class WsFlipped(result: String, outcome: String, win: Int, name: String = "flipped") extends WsOutbound

case class WsStatusUpdated(status: String, name: String = "status-updated") extends WsOutbound

case class WsNewRoundStarted(round: Int, name: String = "new-round-started") extends WsOutbound

//json

object Attach {
  implicit val format: OFormat[Attach] = Json.format[Attach]
}

object Detach {
  implicit val format: OFormat[Detach] = Json.format[Detach]
}

object FlipCoin {
  implicit val format: OFormat[FlipCoin] = Json.format[FlipCoin]
}

object StartNewRound {
  implicit val format: OFormat[StartNewRound] = Json.format[StartNewRound]
}

object WsInbound {
  val w: OWrites[WsInbound] = {
    case c: Attach => Attach.format.writes(c)
    case c: Detach => Detach.format.writes(c)
    case c: FlipCoin => FlipCoin.format.writes(c)
    case c: StartNewRound => StartNewRound.format.writes(c)
  }
  val r: Reads[WsInbound] = (json: JsValue) => {
    (json \ "name").as[String] match {
      case "attach" => Attach.format.reads(json)
      case "detach" => Detach.format.reads(json)
      case "flip-coin" => FlipCoin.format.reads(json)
      case "start-new-round" => StartNewRound.format.reads(json)
    }
  }
  implicit val format: OFormat[WsInbound] = OFormat(r, w)
}

object WsAttached {
  implicit val format: OFormat[WsAttached] = Json.format[WsAttached]
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
    case wso: WsBalanceUpdated => WsBalanceUpdated.format.writes(wso)
    case wso: WsBetAccepted => WsBetAccepted.format.writes(wso)
    case wso: WsFlipped => WsFlipped.format.writes(wso)
    case wso: WsStatusUpdated => WsStatusUpdated.format.writes(wso)
    case wso: WsNewRoundStarted => WsNewRoundStarted.format.writes(wso)

  }
  val r: Reads[WsOutbound] = (json: JsValue) => {
    (json \ "name").as[String] match {
      case "attached" => WsAttached.format.reads(json)
      case "balance-updated" => WsBalanceUpdated.format.reads(json)
      case "bet-accepted" => WsBetAccepted.format.reads(json)
      case "flipped" => WsFlipped.format.reads(json)
      case "status-updated" => WsStatusUpdated.format.reads(json)
      case "new-round-started" => WsNewRoundStarted.format.reads(json)
    }
  }
  implicit val format: OFormat[WsOutbound] = OFormat(r, w)
}

//state
sealed trait FlipStatus

case object BetsAwaiting extends FlipStatus

case class CollectingBets(pendingRequest: PendingRequest) extends FlipStatus

case class PayingOut(pendingRequest: PendingRequest) extends FlipStatus

case object RoundFinished extends FlipStatus

case class FlipResult(outcome: String, win: Int)

case class FlipActorProps(playerId: String)

object FlipState {
  def initial = FlipState(0, BetsAwaiting)
}

case class FlipBet(amount: Int, alternative: String)

case class FlipState(roundId: Int,
                     status: FlipStatus,
                     bet: Option[FlipBet] = None,
                     result: Option[FlipResult] = None) {
  def verify(wc: WalletConfirmation): WalletConfirmation = {
    pendingRequest match {
      case Some(p) if p.walletRequest.id == wc.id =>
        wc
      case _ =>
        throw new IllegalStateException("Request and response doesn't match")
    }
  }

  def pendingRequest: Option[PendingRequest] = status match {
    case BetsAwaiting => None
    case CollectingBets(p) => Some(p)
    case PayingOut(p) => Some(p)
    case RoundFinished => None
  }

  def placeBet(amount: Int, alternative: String): FlipState =
    copy(bet = Some(FlipBet(amount, alternative)))

  def gotoCollectingBets(session: String, ts: Instant)(implicit props: FlipActorProps): FlipState = {
    val p = PendingRequest(WalletRequest(s"${props.playerId}.$roundId.BET", "BET", bet.get.amount, ts))
    copy(status = CollectingBets(p))
  }

  def gotoPayingOut(r: String, ts: Instant)(implicit props: FlipActorProps): FlipState = {
    val win = if (r == bet.get.alternative) bet.get.amount * 2 else 0
    val p = PendingRequest(WalletRequest(s"${props.playerId}.$roundId.WIN", "WIN", win, ts))
    copy(status = PayingOut(p), result = Some(FlipResult(r, win)))
  }

  def gotoRoundFinished: FlipState = copy(status = RoundFinished)

  def gotoBetsAwaiting: FlipState = copy(status = BetsAwaiting, bet = None, result = None, roundId = roundId + 1)

  def handleCommand(implicit session: String, rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] =
    status match {
      case BetsAwaiting => BetsAwaitingBehaviour.handleCommand(this)
      case CollectingBets(_) => CollectingBetsBehaviour.handleCommand(this)
      case PayingOut(_) => PayingOutBehaviour.handleCommand(this)
      case RoundFinished => RoundFinishedBehaviour.handleCommand(this)
    }

  def handleEvent(props: FlipActorProps): PartialFunction[FlipEvent, FlipState] =
    status match {
      case BetsAwaiting => BetsAwaitingBehaviour.handleEvent(this)(props)
      case CollectingBets(_) => CollectingBetsBehaviour.handleEvent(this)(props)
      case PayingOut(_) => PayingOutBehaviour.handleEvent(this)(props)
      case RoundFinished => RoundFinishedBehaviour.handleEvent(this)(props)
    }

}

//behaviour
sealed trait FlipBehaviour {
  def handleCommand(state: FlipState)(implicit session: String, rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]]

  def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState]
}

object BetsAwaitingBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = {
    case FlipCoin(bet, alternative, _) =>
      if (bet <= 0 || bet > 5) {
        Left(FlipError("error.invalid.bet.value"))
      } else if (alternative != "head" && alternative != "tail") {
        Left(FlipError("error.invalid.bet.alternative"))
      } else {
        Right(BetsAccepted(session, bet, alternative, ts))
      }
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case BetsAccepted(session, amount, alternative, ts) =>
      state
        .placeBet(amount, alternative)
        .gotoCollectingBets(session, ts)
  }
}

object CollectingBetsBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = {
    case wc: WalletConfirmation =>
      val confirmation = state.verify(wc)
      val result = if (rng.next(1) == 0) "head" else "tail"
      Right(BetsConfirmed(confirmation, result, ts))

    case WalletError4xx(code) => ???

    case WalletFailure5xx(code) => ???
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case BetsConfirmed(_, result, ts) =>
      state
        .gotoPayingOut(result, ts)
  }
}

object PayingOutBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = {
    case wc: WalletConfirmation =>
      val confirmation = state.verify(wc)
      Right(WinConfirmed(confirmation, ts))

    case WalletError4xx(code) => ???

    case WalletFailure5xx(code) => ???
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case WinConfirmed(_, _) =>
      state
        .gotoRoundFinished
  }
}

object RoundFinishedBehaviour extends FlipBehaviour {
  override def handleCommand(state: FlipState)(implicit session: String, rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, FlipEvent]] = {
    case StartNewRound(_) =>
      Right(NewRoundStarted(ts))
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case NewRoundStarted(_) =>
      state
        .gotoBetsAwaiting
  }
}