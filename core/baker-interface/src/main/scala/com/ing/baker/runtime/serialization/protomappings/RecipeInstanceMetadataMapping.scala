package com.ing.baker.runtime.serialization.protomappings

import com.ing.baker.il.CompiledRecipeId
import com.ing.baker.runtime.serialization.ProtoMap.versioned
import com.ing.baker.runtime.akka.actor.{protobuf => proto}
import com.ing.baker.runtime.scaladsl.RecipeInstanceMetadata
import com.ing.baker.runtime.serialization.ProtoMap

import scala.util.Try

class RecipeInstanceMetadataMapping extends ProtoMap[RecipeInstanceMetadata, proto.RecipeInstanceMetadata] {

    val companion = proto.RecipeInstanceMetadata

    def toProto(a: RecipeInstanceMetadata): proto.RecipeInstanceMetadata = {
      proto.RecipeInstanceMetadata(a.recipeId.v1Value, a.recipeId.v2Value, Some(a.recipeInstanceId), Some(a.createdTime))
    }

    def fromProto(message: proto.RecipeInstanceMetadata): Try[RecipeInstanceMetadata] =
      for {
        recipeId <- versioned(CompiledRecipeId.build(message.recipeId, message.recipeIdV2), "recipeId")
        recipeInstanceId <- versioned(message.recipeInstanceId, "recipeInstanceId")
        createdTime <- versioned(message.createdTime, "createdTime")
      } yield RecipeInstanceMetadata(recipeId, recipeInstanceId, createdTime)
  }
