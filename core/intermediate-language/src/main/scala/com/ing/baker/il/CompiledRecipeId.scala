package com.ing.baker.il

import com.ing.baker.il.CompiledRecipeId.Version2
import com.ing.baker.il.petrinet.{HasCustomToStringForRecipeId, Place, RecipePetriNet}
import com.ing.baker.petrinet.api.Marking

import scala.collection.immutable.Seq
import scala.concurrent.duration.FiniteDuration

sealed trait CompiledRecipeId {
  def v1Value : Option[String]
  def v2Value : Option[String]
}
case class BackwardsCompatibleRecipeId(recipeIdV1: String, recipeIdV2: String) extends CompiledRecipeId {
  override def v1Value: Option[String] = Some(recipeIdV1)
  override def v2Value: Option[String] = Some(recipeIdV2)

  override def equals(obj: Any): Boolean = obj match {
    case BackwardsCompatibleRecipeId(otherV1, otherV2) => recipeIdV1 == otherV1 && recipeIdV2 == otherV2
    case other : SingleVersionRecipeId => recipeIdV1 == other.value || recipeIdV2 == other.value
    case _ => false
  }

  override def toString: String = s"$recipeIdV2-$recipeIdV1"
}
case class SingleVersionRecipeId(value : String) extends CompiledRecipeId {
  def isVersion2 : Boolean = value.startsWith(Version2.prefix)

  override def equals(obj: Any): Boolean = obj match {
    case BackwardsCompatibleRecipeId(otherV1, otherV2) => value == otherV1 || value == otherV2
    case SingleVersionRecipeId(otherValue) => value == otherValue
    case _ => false
  }

  override def toString: String = value

  override def v1Value: Option[String] = if (isVersion2) None else Some(value)
  override def v2Value: Option[String] = if (isVersion2) Some(value) else None
}

object CompiledRecipeId {

  sealed trait RecipeIdVariant
  sealed trait Version1 extends RecipeIdVariant
  case object Scala212CompatibleJava extends Version1
  case object Scala212CompatibleScala extends Version1
  case object Version2 extends RecipeIdVariant {
    def prefix : String = "v2-"
  }

  def fromString(s : String) : Option[CompiledRecipeId] = {
    s.split('-').toList match {
      case singleVersionValue :: Nil => Some(SingleVersionRecipeId(singleVersionValue))
      case version2Value :: version1Value :: Nil => Some(BackwardsCompatibleRecipeId(version1Value, version2Value))
      case _ => None
    }
  }

  def build(recipeIdV1: Option[String], recipeIdV2: Option[String]) : Option[CompiledRecipeId] = (recipeIdV1, recipeIdV2) match {
    case (None, None) => None
    case (Some(v1), Some(v2)) => Some(BackwardsCompatibleRecipeId(v1, v2))
    case (Some(v1), None) => Some(SingleVersionRecipeId(v1))
    case (None, Some(v2)) => Some(SingleVersionRecipeId(v2))
  }

  def calculate(name: String, petriNet: RecipePetriNet, initialMarking: Marking[Place], validationErrors: Seq[String],
                eventReceivePeriod: Option[FiniteDuration], retentionPeriod: Option[FiniteDuration], oldRecipeIdVariant: Option[Version1]) : CompiledRecipeId = {

    /**
      * This calculates a SHA-256 hash for a deterministic string representation of the recipe.
      *
      * For the purpose of data integrity it is enough to truncate to 64 bits:
      *
      * - It is acceptable to truncate in SHA-2 hashes (SHA384 is officially defined as a truncated SHA512)
      *
      * - According to the Birthday Problem, as long as the number of keys is significantly less then 2 32
      * then you need not worry about collisions.
      *
      * Also see the collision table at: https://en.wikipedia.org/wiki/Birthday_attack
      *
      * For example, there is a 1 in a million change of collision when number of recipes reach 6 million
      */
    def calculateRecipeId(variant: RecipeIdVariant): String = {
      val petriNetId: String = petriNet.places.toList.sortBy(_.id).mkString +
        petriNet.transitions.toList.sortBy(_.id).mapRecipeIdStrings(variant).mkString

      val initMarkingId: String = initialMarking.toList.sortBy {
        case (place, _) => place.id
      }.map {
        case (_, tokens) => tokens.toList.sortBy {
          case (tokenData: String, _) => tokenData
          case _ => throw new UnsupportedOperationException("Only string tokens are supported")
        }
      }.toString

      val recipeString = StringBuilder.newBuilder +
        name +
        petriNetId +
        initMarkingId +
        validationErrors.mkString +
        eventReceivePeriod.toString + retentionPeriod

      val prefix = if (variant == Version2) Version2.prefix else ""

      // truncate to 64 bits = 16 hex chars
      prefix + zeroPaddedSHA256(recipeString).substring(0, 16)
    }

    val recipeIdV2 = calculateRecipeId(Version2)
    // the recipe id is a hexadecimal format of the hashcode
    oldRecipeIdVariant match {
      case Some(variant) => BackwardsCompatibleRecipeId(calculateRecipeId(variant), recipeIdV2)
      case None => SingleVersionRecipeId(recipeIdV2)
    }
  }



  implicit class ToRecipeIdStringHelper[A](s : Seq[A]) {
    def mapRecipeIdStrings(variant: RecipeIdVariant) : Seq[String] =
      s.map{
        case o : HasCustomToStringForRecipeId => o.toStringForRecipeId(variant)
        case o => o.toString
      }

    def mkStringForRecipeId(dataStructureName: String, variant: RecipeIdVariant): String =
      mapRecipeIdStrings(variant).mkString(
        start = s"$dataStructureName(",
        sep = ", ",
        end = ")"
      )

    private def toRecipeType(variant: RecipeIdVariant,
                             emptyNameJava : String, nonEmptyNameJava: String,
                             emptyNameScala: String, nonEmptyNameScala: String) : String =
      variant match {
        case Version2 | _ if s.isInstanceOf[List[A]] =>
          s.mkStringForRecipeId("List", variant)
        case Scala212CompatibleJava =>
          if (s.isEmpty) s"$emptyNameJava()"
          else s.mkStringForRecipeId(nonEmptyNameJava, variant)
        case Scala212CompatibleScala =>
          if (s.isEmpty) s"$emptyNameScala()"
          else s.mkStringForRecipeId(nonEmptyNameScala, variant)
      }

    def toRecipeIdStringTypeA(variant: RecipeIdVariant) : String =
      toRecipeType(variant,
        emptyNameJava = "ArraySeq", nonEmptyNameJava = "ArrayBuffer",
        emptyNameScala = "ArraySeq", nonEmptyNameScala = "ArrayBuffer")

    def toRecipeIdStringTypeB(variant: RecipeIdVariant) : String =
      toRecipeType(variant,
        emptyNameJava = "ArraySeq", nonEmptyNameJava = "ArraySeq",
        emptyNameScala = "ArrayBuffer", nonEmptyNameScala = "ArraySeq")
  }
}
