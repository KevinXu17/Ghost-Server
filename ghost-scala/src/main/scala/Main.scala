import java.util.concurrent.Future

import akka.actor.ActorSystem
import akka.http.javadsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes.MovedPermanently
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer

import scala.util.Random
import scala.concurrent.Await
import scala.util.{Failure, Success}

class Ghost(size: Int) {
  private var x_c: Int = scala.util.Random.nextInt(size)
  private var y_c: Int = scala.util.Random.nextInt(size)

  def update() {
    x_c = scala.util.Random.nextInt(size)
    y_c = scala.util.Random.nextInt(size)
  }

  def getX() = {
    x_c
  }

  def getY() = {
    y_c
  }
}

class Town(size: Int) {
  val ghostNumber = scala.util.Random.nextInt(8)+2
  private var ghosts = new Array[Ghost](ghostNumber);
  for( a <- 1 until ghostNumber){
    ghosts(a) = new Ghost(size)
  }
//  for( a <- 1 until ghostNumber){
//    print(s"(${ghosts(a).getX()}, ${ghosts(a).getY()})")
//  }
}


object Main extends App {
  var host = "0.0.0.0"
  var port = 3001
  var SIZE = 200
  var town = new Town(SIZE)

  implicit val system: ActorSystem = ActorSystem(name = "ghost-server")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import system.dispatcher
  import akka.http.scaladsl.server.Directives._

val route = {
  get {
    pathSingleSlash {
      complete("The Ghost Server is up and running, plese use the following commands:\n" +
        "1. Get the ghost location-> use URI: ghost/location\n" +
        "2. Update the ghost location in random-> use URI: ghost/update\n" +
        "3. Calculate the safe path for commuting (for example) from (10, 20) to (20, 30)-> use URI:navi/10/20/20/30")
    }~
    pathPrefix("ghost") {
      get {
        path("location") {
          complete("BOOM HERE IS THE LOCATION FOR YOU!")
        } ~
          path("update") {
            complete("BOOM HERE IS THE UPDATE FOR YOU!")
          }
      }
    } ~
      path("navi" / Remaining) { pathRest =>
        val pos = raw"(\d{1,3})/(\d{1,3})/(\d{1,3})/(\d{1,3})".r
        try {
          val res = pathRest match {
            case pos(s1, s2, e1, e2) => s"Start navigation for ($s1,$s2) and ($e1, $e2)"
          }
          complete(res)
        } catch {
          case e: Exception => complete("Please enter the starting/end point in correct form")
        }
      } ~
      path("crash") {
        sys.error("BOOM!")
      }
  }
}

  var binding = Http().bindAndHandle(route, host, port)

  binding.onComplete {
    case Success(_) => println("Success")
    case Failure(error) => println(s"Failed: ${error.getMessage}")
  }

  import scala.concurrent.duration._
  Await.result(binding, 3.seconds)


}
