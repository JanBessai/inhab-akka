import domain._
import akka.typed._
import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._

object InhabitationAlgorithm extends App {
  sealed trait Result
  final case object Inhabited extends Result

  sealed trait Failure extends Result
  final case class ManyFailures(fails: Seq[Failure]) extends Failure
  final case class Impossible(target: Type) extends Failure
  final case class Cycle(context: Context, target: Type) extends Failure

  sealed trait Request
  final case class Inhabit(
    context: Context,
    target: Type,
    replyTo: ActorRef[Result],
    checkSeen: Option[ActorRef[CheckSeen]] = None) extends Request
  final case class CheckSeen(target: Type, notifyIfSeen: ActorRef[Cycle]) extends Request

  def inhabit: Behavior[Inhabit] =
    Actor.immutable[Inhabit] { (actorContext, request) =>
      request.checkSeen.foreach(_ ! CheckSeen(request.target, request.replyTo))
      request.target match {
        case Arrow(src, tgt) =>
          val nextContext = request.context.merge(src)
          val step = actorContext.spawnAnonymous(forwardResultStep(request.replyTo))
          val nextChecker = actorContext.spawnAnonymous(check(request.context, request.target, request.checkSeen))
          actorContext.self ! Inhabit(request.context.merge(src), tgt, step, Some(nextChecker))
          Actor.same
        case x : Constant =>
          val step = actorContext.spawnAnonymous(existsStep(request.context.typeAssumptions.size, request.replyTo))
          val nextChecker = actorContext.spawnAnonymous(check(request.context, request.target, request.checkSeen))
          for (assumption <- request.context.typeAssumptions) {
            actorContext.spawnAnonymous(matchAssumption(assumption)) ! Inhabit(request.context, x, step, Some(nextChecker))
          }
          Actor.empty
      }
    }

  def check(currentContext: Context, currentTarget: Type, parent: Option[ActorRef[CheckSeen]]): Behavior[CheckSeen] =
    Actor.immutable[CheckSeen]{ (actorContext, toCheck) =>
      if (currentTarget == toCheck.target) {
        toCheck.notifyIfSeen ! Cycle(currentContext, currentTarget)
      }
      parent.foreach(_ ! toCheck)
      Actor.same
    }

  def matchAssumption(ofType: Type): Behavior[Inhabit] =
    Actor.immutable[Inhabit] { (actorContext, request) =>
      ofType.splitAt(request.target) match {
        case Some(Nil) =>
          request.replyTo ! Inhabited
        case Some(sources) =>
          val step = actorContext.spawnAnonymous(allStep(sources.size, request.replyTo))
          for (source <- sources) {
            actorContext.spawnAnonymous(inhabit) ! Inhabit(request.context, source, step, request.checkSeen)
          }
        case None =>
          request.replyTo ! Impossible(request.target)
      }
      Actor.empty
    }



  def forwardResultStep(replyTo: ActorRef[Result]): Behavior[Result] =
    Actor.immutable[Result]{ (actorContext, result) =>
      replyTo ! result
      Actor.empty
    }

  def existsStep(waitFor: Int, replyTo: ActorRef[Result], fails: Seq[Failure] = Seq.empty): Behavior[Result] =
    Actor.immutable[Result]{ (actorContext, result) =>
      result match {
        case Inhabited =>
          replyTo ! result
          Actor.empty
        case fail: Failure =>
          if (waitFor > 1) {
            Actor.deferred(_ => existsStep(waitFor - 1, replyTo, fail +: fails))
          } else {
            replyTo ! ManyFailures(fail +: fails)
            Actor.empty
          }
      }
    }
  def allStep(waitFor: Int, replyTo: ActorRef[Result]): Behavior[Result] =
    Actor.immutable[Result]{ (actorContext, result) =>
      result match {
        case fail: Failure =>
          replyTo ! fail
          Actor.empty
        case Inhabited =>
          if (waitFor > 1) {
            Actor.deferred(_ => allStep(waitFor - 1, replyTo))
          } else {
            replyTo ! Inhabited
            Actor.empty
          }
      }
    }

  import scala.concurrent.ExecutionContext.Implicits.global


  val sigma = Constant("sigma")
  val tau = Constant("tau")
  val rho = Constant("rho")

  val c1 = Constant("c1")
  val c2 = Constant("c2")
  val c3 = Constant("c3")

  def tuple(x: Type, y: Type, r: Type) =
    Arrow(
      Arrow(x, Arrow(y, r)),
      r
    )

  val requests: Seq[(Context, Type)] = Seq(
    (Context(),
      Arrow(
        Arrow(sigma, Arrow(rho, tau)),
        Arrow(Arrow(sigma, rho),
          Arrow(sigma, tau))
      )),
    (Context(Map(Variable("r") -> rho)),
        Arrow(
          Arrow(sigma, Arrow(rho, tau)),
          Arrow(sigma, Arrow(rho, tau))
        )),
    (Context(),
        Arrow(
          Arrow(sigma, Arrow(rho, tau)),
          Arrow(sigma, tau)
        )),
    (Context(Map(Variable("r") -> Arrow(rho, rho))),
        Arrow(
          Arrow(sigma, Arrow(rho, tau)),
          Arrow(sigma, tau)
        )),
    (Context(),
        Arrow(
          Arrow(sigma, tau),
          Arrow(
            Arrow(tau, rho),
            Arrow(sigma, rho)
          )
        ))
  )


  for ((context, target) <- requests) {
    implicit val system: ActorSystem[Inhabit] = ActorSystem("algo", inhabit)
    implicit val timeout: Timeout = 30.seconds
    implicit val scheduler = system.scheduler
    val future: Future[Result] = system ? (Inhabit(context, target, _))
    for {
     result <- future.recover { case ex ⇒ ex.getMessage }
     _ <- { println(s"request: ${(context, target)} result: $result"); system.terminate() }
    } println("system terminated")
  }

}