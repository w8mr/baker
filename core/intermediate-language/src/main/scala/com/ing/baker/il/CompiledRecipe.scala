package com.ing.baker.il

import com.ing.baker.il.CompiledRecipeId.Version1
import com.ing.baker.il.petrinet.{EventTransition, InteractionTransition, Place, RecipePetriNet}
import com.ing.baker.petrinet.api.Marking

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

object CompiledRecipe {

  def build(name: String, petriNet: RecipePetriNet, initialMarking: Marking[Place], validationErrors: Seq[String],
            eventReceivePeriod: Option[FiniteDuration], retentionPeriod: Option[FiniteDuration], oldRecipeIdVariant: Option[Version1] = None): CompiledRecipe = {

    val recipeId = CompiledRecipeId.calculate(name, petriNet, initialMarking, validationErrors, eventReceivePeriod, retentionPeriod, oldRecipeIdVariant)
    CompiledRecipe(name, recipeId, petriNet, initialMarking, validationErrors, eventReceivePeriod, retentionPeriod)
  }
}

/**
  * A Compiled recipe.
  */

case class CompiledRecipe(name: String,
                          recipeId: CompiledRecipeId,
                          petriNet: RecipePetriNet,
                          initialMarking: Marking[Place],
                          validationErrors: Seq[String] = Seq.empty,
                          eventReceivePeriod: Option[FiniteDuration],
                          retentionPeriod: Option[FiniteDuration]) {

  def sensoryEvents: Set[EventDescriptor] = petriNet.transitions.collect {
    case EventTransition(eventDescriptor, true, _) => eventDescriptor
  }

  def getValidationErrors: java.util.List[String] = validationErrors.toList.asJava

  /**
    * Visualise the compiled recipe in DOT format
    *
    * @return
    */
  def getRecipeVisualization: String =
    RecipeVisualizer.visualizeRecipe(this, RecipeVisualStyle.default)

  /**
    * Visualise the compiled recipe in DOT format
    *
    * @return
    */
  def getRecipeVisualization(style: RecipeVisualStyle): String =
    RecipeVisualizer.visualizeRecipe(this, style)

  /**
    * Visualise the compiled recipe in DOT format
    *
    * @param filterFunc
    * @return
    */
  def getFilteredRecipeVisualization(filterFunc: String => Boolean, style: RecipeVisualStyle = RecipeVisualStyle.default): String =
    RecipeVisualizer.visualizeRecipe(this, style, filter = filterFunc)


  def getFilteredRecipeVisualization(filter: String): String =
    getFilteredRecipeVisualization(x => !x.contains(filter))

  /**
    * Returns a DOT (http://www.graphviz.org/) representation of the recipe.
    * All events/interaction/ingredients that contain one of the given filter strings are filtered out
    *
    * @param filters
    * @return
    */
  def getFilteredRecipeVisualization(filters: Array[String]): String =
    getFilteredRecipeVisualization((current) => filters.forall(filter => !current.contains(filter)))

  /**
    * Visualises the underlying petri net in DOT format
    *
    * @return
    */
  def getPetriNetVisualization: String = RecipeVisualizer.visualizePetriNet(petriNet.innerGraph)

  val interactionTransitions: Set[InteractionTransition] = petriNet.transitions.collect {
    case t: InteractionTransition => t
  }

  val interactionEvents: Set[EventDescriptor] = interactionTransitions flatMap (it => it.eventsToFire)

  val allEvents: Set[EventDescriptor] = sensoryEvents ++ interactionEvents

  def getAllEvents: java.util.Set[EventDescriptor] = allEvents.asJava

  val allIngredients: Set[IngredientDescriptor] = allEvents.flatMap {
    events => events.ingredients
  }

  def getAllIngredients: java.util.Set[IngredientDescriptor] = allIngredients.asJava
}
