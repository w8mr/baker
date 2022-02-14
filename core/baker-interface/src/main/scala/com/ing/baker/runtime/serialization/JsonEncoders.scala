package com.ing.baker.runtime.serialization

import java.util.Base64
import com.ing.baker.il.{BackwardsCompatibleRecipeId, CompiledRecipe, CompiledRecipeId, SingleVersionRecipeId}
import com.ing.baker.il.failurestrategy.ExceptionStrategyOutcome
import com.ing.baker.runtime.common.{BakerException, RejectReason, SensoryEventStatus}
import com.ing.baker.runtime.scaladsl._
import com.ing.baker.runtime.serialization.JsonCodec._
import com.ing.baker.types
import io.circe.Encoder._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import scala.collection.JavaConverters._

object JsonEncoders {

  private[serialization] val typeFieldName: String = "typ"
  private[serialization] val subTypeFieldName: String = "styp"
  private[serialization] val valueFieldName: String = "val"

  private[serialization] val byteArraySubtype: String = "ByteArray"

  private[serialization] val nullValueType: Int = 0
  private[serialization] val listValueType: Int = 1
  private[serialization] val recordValueType: Int = 2
  private[serialization] val primitiveValueType: Int = 3

  implicit val valuesEncoder: Encoder[types.Value] =
    Encoder.instance {
      case types.NullValue =>
        Json.fromFields(List((typeFieldName, nullValueType.asJson)))
      case types.ListValue(entries) =>
        Json.fromFields(List(
          (typeFieldName, listValueType.asJson),
          (valueFieldName, encodeList(valuesEncoder)(entries))))
      case types.RecordValue(entries) =>
        Json.fromFields(List(
          (typeFieldName, recordValueType.asJson),
          (valueFieldName, encodeMap(KeyEncoder.encodeKeyString, valuesEncoder)(entries))))
      case types.PrimitiveValue(value) =>
        val (subType, realValue) = value match {
          case bytes: Array[Byte] =>
            (byteArraySubtype, Base64.getEncoder.encodeToString(bytes))
          case _ => (value.getClass.getName, value.toString)
        }

        Json.fromFields(List(
          (typeFieldName, primitiveValueType.asJson),
          (subTypeFieldName, subType.asJson),
          (valueFieldName, realValue.asJson)))
    }

  implicit val eventInstanceEncoder: Encoder[EventInstance] = deriveEncoder[EventInstance]
  implicit val eventMomentEncoder: Encoder[EventMoment] = deriveEncoder[EventMoment]

  implicit val rejectReasonEncoder: Encoder[RejectReason] = encodeString.contramap(_.toString)
  implicit val exceptionEncoder: Encoder[ExceptionStrategyOutcome] = deriveEncoder[ExceptionStrategyOutcome]
  implicit val throwableEncoder: Encoder[Throwable] = (throwable: Throwable) => Json.obj(("error", Json.fromString(throwable.getMessage)))

  implicit val singleVersionRecipeIdEncoder : Encoder[SingleVersionRecipeId] = Encoder.encodeString.contramap(_.value)

  private def encodeRecipeIdMergeObject(compiledRecipeId: CompiledRecipeId) : Json = compiledRecipeId match {
    case BackwardsCompatibleRecipeId(recipeIdV1, recipeIdV2) =>
      Json.obj(
        "recipeId" -> recipeIdV1.asJson,
        "recipeIdV2" -> recipeIdV2.asJson,
      )
    case SingleVersionRecipeId(value) =>
      Json.obj(
        "recipeId" -> value.asJson,
      )
  }

  implicit val compiledRecipeEncoder: Encoder[CompiledRecipe] =
  // TODO: write PetriNet and Marking to json
    (recipe: CompiledRecipe) => Json.obj(
      "name" -> recipe.name.asJson,
      "validationErrors" -> recipe.getValidationErrors.asScala.asJson
    ).deepMerge(encodeRecipeIdMergeObject(recipe.recipeId))


  //Encoder for baker events.
  //The structure of circe means you cannot encode a single value (CompiledRecipeId) into 2 json values (recipeId and recipeIdV2) automatically.
  //Therefore we encode it as a single value first (recipeId) and then add the second value afterwards (recipeIdV2)
  implicit val bakerEventEncoder : Encoder[BakerEvent] = Encoder.instance{ be : BakerEvent =>
    implicit val recipeIdEncoder : Encoder[CompiledRecipeId] = {
      // Only encode recipeIdV1, so the other version can be added later
      case BackwardsCompatibleRecipeId(recipeIdV1, _) => Json.fromString(recipeIdV1)
      case SingleVersionRecipeId(value) => Json.fromString(value)
    }

    // BakerEvent encoder which includes the recipeId (v1)
    val bakerEventEncoderWithOldRecipeId : Encoder[BakerEvent] = deriveEncoder[BakerEvent]
    // Retrieve the recipeIdV2 if filled.
    val recipeIdV2 : Option[String] = be match {
      case event : ContainsCompiledRecipeId => event.recipeId match {
        case rid : BackwardsCompatibleRecipeId => Some(rid.recipeIdV2)
        case _ => None
      }
      case _ => None
    }
    // Create an object with the recipeIdV2 value.
    val recipeIdV2Object = recipeIdV2.map(riv2 => Json.obj("recipeIdV2" -> Json.fromString(riv2))).getOrElse(Json.obj())
    // Merge the bakerEvent and the recipeIdV2 fields
    be.asJson(bakerEventEncoderWithOldRecipeId).deepMerge(recipeIdV2Object)
  }

  implicit val sensoryEventStatusEncoder: Encoder[SensoryEventStatus] = (s: SensoryEventStatus) => Encoder.encodeString.apply(s.toString)
  implicit val sensoryEventResultEncoder: Encoder[SensoryEventResult] = deriveEncoder[SensoryEventResult]
  implicit val bakerResultEncoder: Encoder[BakerResult] = deriveEncoder[BakerResult]
  implicit val bakerExceptionEncoder: Encoder[BakerException] = (e: BakerException) => {
    val (message, enum) = BakerException.encode(e)
    Json.obj(
      "enum" -> `enum`.asJson,
      "message"-> message.asJson
    )
  }

  implicit val interactionInstanceInputEncoder: Encoder[InteractionInstanceInput] =  deriveEncoder[InteractionInstanceInput]
  implicit val interactionInstanceDescriptorEncoder: Encoder[InteractionInstanceDescriptor] = deriveEncoder[InteractionInstanceDescriptor]
  implicit val optionalInteractionInstanceDescriptorEncoder: Encoder[Option[InteractionInstanceDescriptor]] = Encoder.encodeOption(interactionInstanceDescriptorEncoder)

  implicit val recipeEnoder: Encoder[EncodedRecipe] = deriveEncoder[EncodedRecipe]
}
