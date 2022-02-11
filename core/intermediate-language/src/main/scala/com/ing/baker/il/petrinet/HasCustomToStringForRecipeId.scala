package com.ing.baker.il.petrinet

import com.ing.baker.il.CompiledRecipeId.RecipeIdVariant

trait HasCustomToStringForRecipeId {
  def toStringForRecipeId(recipeIdVariant: RecipeIdVariant): String = this.toString
}
