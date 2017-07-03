import akka.typed._
import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._

object HelloWorld {
  sealed trait Message
  case class Hello(myName : String, replyTo: ActorRef[Reply]) extends Message
  case class Reply(text : String) extends Message

  val server: Behavior[Hello] =
    Actor.immutable[Hello] { (actorContext, message) =>
      message.replyTo ! Reply(s"Hi, ${message.myName}!")
      Actor.same
    }

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val system: ActorSystem[Hello] = ActorSystem("hello", server)
  implicit val timeout: Timeout = 30.seconds
  implicit val scheduler = system.scheduler
  val future: Future[Reply] = system ? (replyTo => Hello("KSOS", replyTo))
  for {
    result <- future.recover { case ex ⇒ ex.getMessage }
    _ <- { println(s"$result"); system.terminate() }
  }
    println("system terminated")






  /*implicit val system: ActorSystem[Inhabit] = ActorSystem("algo", inhabit)
  implicit val timeout: Timeout = 30.seconds
  implicit val scheduler = system.scheduler
  val future: Future[Result] = system ? (Inhabit(context, target, _))
  for {
    result <- future.recover { case ex ⇒ ex.getMessage }
    _ <- { println(s"request: ${(context, target)} result: $result"); system.terminate() }
  } println("system terminated")*/
}