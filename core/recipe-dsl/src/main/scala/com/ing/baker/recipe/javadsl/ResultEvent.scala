package com.ing.baker.recipe.javadsl

import com.ing.baker.recipe.common

import scala.annotation.{nowarn, varargs}

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

case class ResultEvent private(
                                override val name: String = "",
                                override val requiredEvents: Set[String] = Set.empty,
                                override val requiredOneOfEvents: Set[Set[String]] = Set.empty)
  extends common.ResultEvent {

  // initialize with defaults
  def this(name: String) = this(name = name, requiredEvents = Set.empty, requiredOneOfEvents = Set.empty)

  /**
   * This sets a requirement for this result-event that a specific event needs to have been fired before it can execute.
   *
   * @param newRequiredEvent the class of the events that needs to have been fired
   * @return
   */
  def withRequiredEvent(newRequiredEvent: Class[_]): ResultEvent =
    this.copy(requiredEvents = requiredEvents + newRequiredEvent.getSimpleName)

  /**
   * This sets a requirement for this result-event that some specific events needs to have been fired before it can execute.
   *
   * @param newRequiredEvents the classes of the events.
   * @return
   */
  @SafeVarargs
  @varargs
  def withRequiredEvents(newRequiredEvents: Class[_]*): ResultEvent =
    this.copy(requiredEvents = requiredEvents ++ newRequiredEvents.map(_.getSimpleName))

  /**
   * This sets a requirement for this result-event that some specific events needs to have been fired before it can execute.
   *
   * @param newRequiredEvents the classes of the event.
   * @return
   */
  @nowarn
  def withRequiredEvents(newRequiredEvents: java.util.Set[Class[_]]): ResultEvent =
    this.copy(requiredEvents = requiredEvents ++ newRequiredEvents.asScala.map(_.getSimpleName))


  /**
   * This sets a requirement for this result-event that a specific event needs to have been fired before it can execute.
   *
   * @param newRequiredEventName the name of the events that needs to have been fired
   * @return
   */
  def withRequiredEventFromName(newRequiredEventName: String): ResultEvent =
    this.copy(requiredEvents = requiredEvents + newRequiredEventName)

  /**
   * This sets a requirement for this result-event that some specific events needs to have been fired before it can execute.
   *
   * @param newRequiredEventNames the names of the events.
   * @return
   */
  @SafeVarargs
  @varargs
  def withRequiredEventsFromName(newRequiredEventNames: String*): ResultEvent =
    this.copy(requiredEvents = requiredEvents ++ newRequiredEventNames)

  /**
   * This sets a requirement for this result-event that some specific events needs to have been fired before it can execute.
   *
   * @param newRequiredEvents the names of the events.
   * @return
   */
  @nowarn
  def withRequiredEventsFromName(newRequiredEvents: java.util.Set[String]): ResultEvent =
    this.copy(requiredEvents = requiredEvents ++ newRequiredEvents.asScala)

  /**
   * This sets a requirement for this result-event that one of the given events needs to have been fired before it can execute.
   *
   * @param newRequiredOneOfEvents the classes of the events.
   * @return
   */
  @SafeVarargs
  @varargs
  def withRequiredOneOfEvents(newRequiredOneOfEvents: Class[_]*): ResultEvent = {
    if (newRequiredOneOfEvents.nonEmpty && newRequiredOneOfEvents.size < 2)
      throw new IllegalArgumentException("At least 2 events should be provided as 'requiredOneOfEvents'")

    val newRequired: Set[Set[String]] = requiredOneOfEvents + newRequiredOneOfEvents.map(_.getSimpleName).toSet

    copy(requiredOneOfEvents = newRequired)
  }

  /**
   * This sets a requirement for this result-event that one of the given events needs to have been fired before it can execute.
   *
   * @param newRequiredOneOfEvents the names of the events.
   * @return
   */
  @SafeVarargs
  @varargs
  def withRequiredOneOfEventsFromName(newRequiredOneOfEvents: String*): ResultEvent = {
    if (newRequiredOneOfEvents.nonEmpty && newRequiredOneOfEvents.size < 2)
      throw new IllegalArgumentException("At least 2 events should be provided as 'requiredOneOfEvents'")
    val newRequired: Set[Set[String]] = requiredOneOfEvents + newRequiredOneOfEvents.toSet

    copy(requiredOneOfEvents = newRequired)
  }

}
