package com.ing.baker.baas.state

import java.io.File
import java.nio.file.Files

import cats.implicits._
import cats.effect.{ContextShift, IO, Timer}
import com.ing.baker.il.CompiledRecipe
import com.ing.baker.runtime.akka.actor.protobuf
import com.ing.baker.runtime.common.BakerException.NoSuchRecipeException
import com.ing.baker.runtime.scaladsl.Baker
import com.ing.baker.runtime.serialization.ProtoMap
import org.apache.commons.codec.binary.Base64

import scala.concurrent.duration._

object RecipeLoader {

  def loadRecipesIntoBaker(path: String, baker: Baker)(implicit cs: ContextShift[IO]): IO[Unit] =
    for {
      recipes <- RecipeLoader.loadRecipes(path)
      _ <- if (recipes.isEmpty) IO.raiseError(new RuntimeException(s"No recipes found in the recipe directory ($path)")) else IO.unit
      _ <- recipes.traverse { recipe =>
        IO.fromFuture(IO(baker.addRecipe(recipe)))
      }
    } yield ()

  def loadRecipesIfRecipeNotFound[A](path: String, baker: Baker)(f: IO[A])(implicit timer: Timer[IO], cs: ContextShift[IO]): IO[A] = {
    val time = 1.minute / 12
    val split = 12
    def within(count: Int)(f0: IO[A]): IO[A] = {
      if (count < 1) f0 else f0.attempt.flatMap {
        case Left(_: NoSuchRecipeException) => IO.sleep(time) *> within(count - 1)(loadRecipesIntoBaker(path, baker) *> f)
        case Left(e) => IO.raiseError(e)
        case Right(a) => IO.pure(a)
      }
    }
    within(split)(f)
  }

  def loadRecipes(path: String): IO[List[CompiledRecipe]] = {

    def recipeFiles(path: String): IO[List[File]] = IO {
      val d = new File(path)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List.empty[File]
      }
    }

    for {
      files <- recipeFiles(path)
      recipes <- files.traverse { file =>
        for {
          bytes <- IO(Files.readAllBytes(file.toPath))
          decode64 = Base64.decodeBase64(new String(bytes))
          protoRecipe <- IO.fromTry(protobuf.CompiledRecipe.validate(decode64))
          recipe <- IO.fromTry(ProtoMap.ctxFromProto(protoRecipe))
        } yield recipe
      }
    } yield recipes
  }
}
