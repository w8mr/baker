package com.ing.bakery.baker
import akka.actor.ActorSystem
import cats.effect.{IO, Resource}
import com.typesafe.config.Config
import cats.implicits._

import scala.concurrent.duration._
import cats.effect.Temporal
object TestWatcher {
  var started = false
  var triggered = false
}

class TestWatcher extends Watcher {

  override def trigger(): Unit = {
    TestWatcher.triggered = true
  }

  override def resource(config: Config, system: ActorSystem, cassandra: Option[Cassandra], callbackEnable: () => Unit): Resource[IO, Unit] = {
    implicit val timer: Temporal[IO] = IO.timer(system.dispatcher)
    Resource.eval(IO{
      TestWatcher.started = true
      callbackEnable()
    } >> IO.sleep(100 millis)
      >> IO(TestWatcher.triggered = true) )
  }
}
