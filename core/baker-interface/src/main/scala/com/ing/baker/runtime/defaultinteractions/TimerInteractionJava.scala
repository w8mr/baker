package com.ing.baker.runtime.defaultinteractions

import java.time.Duration
import java.util.concurrent.TimeUnit

import cats.effect.IO

import scala.concurrent.duration.FiniteDuration
import cats.effect.Temporal

class TimerInteractionJava(skipWait: Boolean)(implicit timer: Temporal[IO]) {

  class TimeWaited()

  val name = "TimerInteraction"

  def apply(WaitTime: Duration): IO[TimeWaited] = {
    if(skipWait)
      IO.pure(new TimeWaited)
    else
      IO.sleep(FiniteDuration.apply(WaitTime.toMillis, TimeUnit.MILLISECONDS)) *> IO(new TimeWaited)
  }
}
