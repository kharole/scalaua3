package com.scalaua.services

import java.time.Instant

import scala.collection.immutable
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

case class PendingRequest(walletRequest: WalletRequest, nrOfAttempts: Int = 0, undelivered: Boolean = false) {
  def incNrOfAttempts: PendingRequest = copy(nrOfAttempts = nrOfAttempts + 1)
}

case class WalletBalanceRequest(playerId: String, session: String)

case class WalletRequest(id: String, requestType: String, amount: Int, ts: Instant, playerId: String, session: String)

case class FlipError(code: String)

//commands

sealed trait FlipCommand

sealed trait WalletResponse extends FlipCommand

case class BalanceResponse(balance: Int)

case class WalletConfirmation(id: String, amount: Int, newBalance: Int, processedAt: Instant) extends WalletResponse

case class WalletError4xx(message: String) extends WalletResponse

case class WalletFailure5xx(message: String) extends WalletResponse

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

case class BetAccepted(amount: Int, alternative: String, timestamp: Instant) extends FlipEvent

case class BetConfirmed(confirmation: WalletConfirmation, result: String, outcome: String, win: Int, timestamp: Instant) extends FlipEvent with ConfirmationEvent

case class BetError(reason: WalletError4xx, roundId: Int, timestamp: Instant) extends FlipEvent with FlipWalletError

case class BetAttemptFailed(reason: WalletFailure5xx, timestamp: Instant) extends FlipEvent with FlipAttemptFailed

case class WinConfirmed(confirmation: WalletConfirmation, timestamp: Instant) extends FlipEvent with ConfirmationEvent

case class WinError(reason: WalletError4xx, timestamp: Instant) extends FlipEvent with FlipWalletError

case class WinAttemptFailed(reason: WalletFailure5xx, timestamp: Instant) extends FlipEvent with FlipAttemptFailed

case class NewRoundStarted(roundId: Int, timestamp: Instant) extends FlipEvent

case class Attached(session: String, timestamp: Instant) extends FlipEvent

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
                     result: Option[FlipResult] = None,
                     session: Option[String] = None) {

  def incNrOfAttempts(): FlipState = status match {
    case cb@CollectingBets(p) =>
      copy(status = cb.copy(pendingRequest = p.incNrOfAttempts))
    case po@PayingOut(p) =>
      copy(status = po.copy(pendingRequest = p.incNrOfAttempts))
    case _ =>
      throw new IllegalStateException("incNrOfAttempts only allowed in processing states")
  }

  def attach(newSession: String): FlipState = copy(session = Some(newSession))

  def detach(): FlipState = copy(session = None)

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

  def gotoCollectingBets(ts: Instant)(implicit props: FlipActorProps): FlipState = {
    val p = PendingRequest(WalletRequest(s"${props.playerId}.$roundId.BET", "BET", bet.get.amount, ts, props.playerId, session.get))
    copy(status = CollectingBets(p))
  }

  def gotoPayingOut(result: String, win: Int, ts: Instant)(implicit props: FlipActorProps): FlipState = {
    val p = PendingRequest(WalletRequest(s"${props.playerId}.$roundId.WIN", "WIN", win, ts, props.playerId, session.get))
    copy(status = PayingOut(p), result = Some(FlipResult(result, win)))
  }

  def gotoRoundFinished: FlipState = copy(status = RoundFinished)

  def gotoBetsAwaiting(newRoundId: Int): FlipState = copy(status = BetsAwaiting, bet = None, result = None, roundId = newRoundId)

  def behaviour: FlipBehaviour = {
    val baseBehaviour = status match {
      case BetsAwaiting => BetsAwaitingBehaviour(this)
      case CollectingBets(_) => CollectingBetsBehaviour(this)
      case PayingOut(_) => PayingOutBehaviour(this)
      case RoundFinished => RoundFinishedBehaviour(this)
    }
    val detachBehaviourAspect = AttachDetachBehaviourAspect(this)
    FallbackFlipBehaviour(baseBehaviour, detachBehaviourAspect)
  }
}

//behaviour
sealed trait FlipBehaviour {
  def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]]

  def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState]
}

case class FallbackFlipBehaviour(a: FlipBehaviour, b: FlipBehaviour) extends FlipBehaviour {
  override def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]] =
    a.validateCommand orElse b.validateCommand

  override def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] =
    a.handleEvent orElse b.handleEvent
}

case class AttachDetachBehaviourAspect(state: FlipState) extends FlipBehaviour {
  override def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]] = {
    case Attach(s) => Right(List(Attached(s, ts)))
    case Detach() => Right(List(Detached(ts)))
  }

  override def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case Attached(newSession, _) => state.attach(newSession)
    case Detached(_) => state.detach()
  }
}

case class BetsAwaitingBehaviour(state: FlipState) extends FlipBehaviour {
  override def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]] = {
    case FlipCoin(bet, alternative) =>
      if (bet <= 0 || bet > 5) {
        Left(FlipError("invalid bet value"))
      } else if (alternative != "head" && alternative != "tail") {
        Left(FlipError("invalid bet alternative"))
      } else {
        Right(List(BetAccepted(bet, alternative, ts)))
      }
  }

  override def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case BetAccepted(amount, alternative, ts) =>
      state
        .placeBet(amount, alternative)
        .gotoCollectingBets(ts)
  }
}

case class CollectingBetsBehaviour(state: FlipState) extends FlipBehaviour {
  override def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]] = {
    case wc: WalletConfirmation =>
      val confirmation = state.verify(wc)
      //val result = if (rng.next(2) == 0) "head" else "tail"
      val result = "head"
      val (outcome, win) = if (result == state.bet.get.alternative)
        ("win", state.bet.get.amount * 2)
      else
        ("loss", 0)
      Right(List(BetConfirmed(confirmation, result, outcome, win, ts)))

    case e: WalletError4xx =>
      Right(List(BetError(e, state.roundId + 1, ts)))

    case e: WalletFailure5xx =>
      Right(List(BetAttemptFailed(e, ts)))
  }

  override def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case BetConfirmed(_, result, _, win, ts) =>
      state
        .gotoPayingOut(result, win, ts)

    case BetError(_, newRoundId, _) =>
      state
        .gotoBetsAwaiting(newRoundId)

    case BetAttemptFailed(_, _) =>
      state
        .incNrOfAttempts()
  }
}

case class PayingOutBehaviour(state: FlipState) extends FlipBehaviour {
  override def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]] = {
    case wc: WalletConfirmation =>
      val confirmation = state.verify(wc)
      Right(List(WinConfirmed(confirmation, ts)))

    case e: WalletError4xx =>
      Right(List(WinError(e, ts)))

    case e: WalletFailure5xx =>
      Right(List(WinAttemptFailed(e, ts)))
  }

  override def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case WinConfirmed(_, _) =>
      state
        .gotoRoundFinished

    case WinAttemptFailed(_, _) | WinError(_, _) =>
      state
        .incNrOfAttempts()
  }
}

case class RoundFinishedBehaviour(state: FlipState) extends FlipBehaviour {
  override def validateCommand(implicit rng: Rng, props: FlipActorProps, ts: Instant): PartialFunction[FlipCommand, Either[FlipError, immutable.Seq[FlipEvent]]] = {
    case StartNewRound() =>
      Right(List(NewRoundStarted(state.roundId + 1, ts)))
  }

  override def handleEvent(implicit props: FlipActorProps): PartialFunction[FlipEvent, FlipState] = {
    case NewRoundStarted(newRoundId, _) =>
      state
        .gotoBetsAwaiting(newRoundId)
  }
}