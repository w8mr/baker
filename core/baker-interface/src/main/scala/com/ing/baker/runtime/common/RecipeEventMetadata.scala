package com.ing.baker.runtime.common

import com.ing.baker.il.CompiledRecipeId
import com.ing.baker.runtime.common.LanguageDataStructures.LanguageApi

trait RecipeEventMetadata extends LanguageApi {

  def recipeId: CompiledRecipeId

  def recipeName: String

  def recipeInstanceId: String

}
