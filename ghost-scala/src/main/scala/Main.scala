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
  private var ghosts = new Array[Ghost](10);
  var ghostNumber = 0
  updateGhostLocation()


  def updateGhostLocation() {
    ghostNumber = scala.util.Random.nextInt(8)+2
    for( a <- 0 until ghostNumber){
      ghosts(a) = new Ghost(size)
    }
  }

  def getGhostLocation(): String = {
    var output = s"There are $ghostNumber ghosts in town and they are at:\n"
    for( a <- 0 until ghostNumber){
      var temp = s"(${ghosts(a).getX()}, ${ghosts(a).getY()})\n"
      output = output+temp
    }
    output
  }


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
            complete(town.getGhostLocation())
          } ~
            path("update") {
              town.updateGhostLocation()
              complete("The ghost locations are updated!\n" +
                town.getGhostLocation())
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
