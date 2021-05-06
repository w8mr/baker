package com.ing.baker.runtime.akka.internal

import cats.effect.{ContextShift, IO}
import com.ing.baker.il.{EventDescriptor, IngredientDescriptor}
import com.ing.baker.il.petrinet.InteractionTransition
import com.ing.baker.runtime.scaladsl.{InteractionInstance, InteractionInstanceF}
import com.ing.baker.types
import com.ing.baker.types.Type
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpecLike, AsyncWordSpecLike}
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.ExecutionContext

class LocalInteractionsSpec extends AnyWordSpecLike with Matchers with MockitoSugar {
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  "getImplementation" should {
    "return Some" when {
      "an interaction implementation is available" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        val found = interactionManager.findFor(interactionTransition).unsafeRunSync().get
        found.name shouldBe interactionImplementation.name
        found.input shouldBe interactionImplementation.input
        found.output shouldBe interactionImplementation.output
      }

      "an interaction implementation is available with output defined" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation.output).thenReturn(None)
        when(interactionImplementation.output).thenReturn(Some(Map("outputEvent"-> Map.empty[String, Type])))

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))
        when(interactionTransition.originalEvents).thenReturn(Seq(EventDescriptor("outputEvent", Seq.empty)))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        val found = interactionManager.findFor(interactionTransition).unsafeRunSync().get
        found.name shouldBe interactionImplementation.name
        found.input shouldBe interactionImplementation.input
        found.output shouldBe interactionImplementation.output
      }

      "multiple interaction implementations are available" in {
        val interactionImplementation1 = mock[InteractionInstance]
        when(interactionImplementation1.name).thenReturn("InteractionName")
        when(interactionImplementation1.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation1.output).thenReturn(None)

        val interactionImplementation2 = mock[InteractionInstance]
        when(interactionImplementation2.name).thenReturn("InteractionName2")
        when(interactionImplementation2.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation2.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation1, interactionImplementation2))

        val found = interactionManager.findFor(interactionTransition).unsafeRunSync().get
        found.name shouldBe interactionImplementation1.name
        found.input shouldBe interactionImplementation1.input
        found.output shouldBe interactionImplementation1.output
      }

      "two implementations with the same correct name but only one has the correct input types" in {
        val interactionImplementation1 = mock[InteractionInstance]
        when(interactionImplementation1.name).thenReturn("InteractionName")
        when(interactionImplementation1.input).thenReturn(Seq.empty[Type])
        when(interactionImplementation1.output).thenReturn(None)

        val interactionImplementation2 = mock[InteractionInstance]
        when(interactionImplementation2.name).thenReturn("InteractionName")
        when(interactionImplementation2.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation2.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation1, interactionImplementation2))
        val found = interactionManager.findFor(interactionTransition).unsafeRunSync().get
        found.name shouldBe interactionImplementation2.name
        found.input shouldBe interactionImplementation2.input
        found.output shouldBe interactionImplementation2.output
      }

      "two implementations with the same correct name but only one has the correct output" in {
        val interactionImplementation1 = mock[InteractionInstance]
        when(interactionImplementation1.name).thenReturn("InteractionName")
        when(interactionImplementation1.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation1.output).thenReturn(Some(Map("wrongOutputEvent"-> Map.empty[String, Type])))

        val interactionImplementation2 = mock[InteractionInstance]
        when(interactionImplementation2.name).thenReturn("InteractionName")
        when(interactionImplementation2.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation2.output).thenReturn(Some(Map("outputEvent"-> Map.empty[String, Type])))

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))
        when(interactionTransition.originalEvents).thenReturn(Seq(EventDescriptor("outputEvent", Seq.empty)))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation1, interactionImplementation2))
        val found = interactionManager.findFor(interactionTransition).unsafeRunSync().get
        found.name shouldBe interactionImplementation2.name
        found.input shouldBe interactionImplementation2.input
        found.output shouldBe interactionImplementation2.output
      }
    }

    "return None" when {
      "an interaction implementation has a wrong name" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("WrongInteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        interactionManager.findFor(interactionTransition).unsafeRunSync() shouldBe None
      }

      "an interaction implementation has a wrong ingredient input type" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.CharArray)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        interactionManager.findFor(interactionTransition).unsafeRunSync() shouldBe None
      }

      "an interaction implementation has a wrong output event name" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation.output).thenReturn(Some(Map("wrongOutputEvent"-> Map.empty[String, Type])))

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))
        when(interactionTransition.originalEvents).thenReturn(Seq(EventDescriptor("outputEvent", Seq.empty)))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        interactionManager.findFor(interactionTransition).unsafeRunSync() shouldBe None
      }

      "an interaction implementation has extra ingredient input types" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32, types.CharArray))
        when(interactionImplementation.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        interactionManager.findFor(interactionTransition).unsafeRunSync() shouldBe None
      }

      "an interaction implementation has not enough ingredient input types" in {
        val interactionImplementation = mock[InteractionInstance]
        when(interactionImplementation.name).thenReturn("InteractionName")
        when(interactionImplementation.input).thenReturn(Seq(types.Int32))
        when(interactionImplementation.output).thenReturn(None)

        val interactionTransition = mock[InteractionTransition]
        when(interactionTransition.originalInteractionName).thenReturn("InteractionName")
        val ingredientDescriptor: IngredientDescriptor = IngredientDescriptor("ingredientName", types.Int32)
        val ingredientDescriptor2: IngredientDescriptor = IngredientDescriptor("ingredientName2", types.CharArray)
        when(interactionTransition.requiredIngredients).thenReturn(Seq(ingredientDescriptor, ingredientDescriptor2))

        val interactionManager: LocalInteractions = LocalInteractions(List(interactionImplementation))
        interactionManager.findFor(interactionTransition).unsafeRunSync() shouldBe None
      }

      "empty interaction seq" in {
        val interactionManager: LocalInteractions = LocalInteractions()

        val interactionTransition: InteractionTransition = mock[InteractionTransition]
        interactionManager.findFor(interactionTransition).unsafeRunSync() shouldBe None
      }
    }
  }
}
