package server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.HttpEntity
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global

object Server extends App {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val basePageRoute: Route =
    get(pathSingleSlash(getFromFile("app/assets/static/index.html"))) ~ getFromDirectory("app/assets/static/")

  val fetchTags: Route =
    (get & path("email" / Segment)) { email =>
        complete(HttpEntity(`application/json`, """{ "tags" : ["loadedT","t2"] }"""))
      }

  val saveTags: Route =
    (post & path("save")) {
      complete(HttpEntity(`application/json`, """ok"""))
    }

  case class SearchResult(results: List[String])
  val search: Route =
    (post & path("search")) {
      complete(HttpEntity(`application/json`, SearchResult(List("suche1", "suche2", "suche3")).asJson.noSpaces))
    }

  Http().bindAndHandle(basePageRoute ~ fetchTags ~ saveTags ~ search, "0.0.0.0", 8080)
}