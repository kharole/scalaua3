package com.scalaua.services

import java.time.Instant

import scala.util.{Random, Right}


object Rng {
  private val r = Random

  def real: Rng = (n: Int) => r.nextInt(n)

  def fixed(value: Int): Rng = (_: Int) => value
}

trait Rng {
  def next(n: Int): Int
}

//model

case class PendingRequest(walletRequest: WalletRequest, nrOfAttempts: Int = 0, undelivered: Boolean = false)

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

case class Attach(session: String) extends FlipCommand

case class Detach() extends FlipCommand

case class FlipCoin(bet: Int, alternative: String) extends FlipCommand

case class StartNewRound() extends FlipCommand

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
    case FlipCoin(bet, alternative) =>
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
    case StartNewRound() =>
      Right(NewRoundStarted(ts))
  }

  override def handleEvent(state: FlipState)(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case NewRoundStarted(_) =>
      state
        .gotoBetsAwaiting
  }
}