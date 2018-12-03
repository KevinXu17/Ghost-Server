import java.util.concurrent.Future

import akka.actor.ActorSystem
import akka.http.javadsl.model.StatusCodes
import akka.http.scaladsl.model.StatusCodes.MovedPermanently
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer

import collection.mutable.PriorityQueue
import scala.util.Random
import scala.concurrent.{Await, Future}
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
  var XB = 50
  var YB = 50
  var TownMap = MAP1.m
  var ghosts = new Array[Ghost](10);
  var ghostNumber = 1
  updateGhostLocation()
  var route = this.calculateRoute(10,10,40,30)
  for(i <- 0 until route.length) {
    print(route(i).toString() + "\n")
  }

  def updateGhostLocation() {
    ghostNumber = scala.util.Random.nextInt(8)+2
    for( a <- 0 until ghostNumber){
      ghosts(a) = new Ghost(size)
    }
  }

  def checkGhost(x:Int, y:Int): Boolean = {
    var res = false
    for(i <- 0 until ghostNumber) {
      if(math.abs(ghosts(i).getX()-x)<=2 && math.abs(ghosts(i).getY() - y)<=2) {
        res = true
      }
    }
    res
  }

  def insertChar(s: String, index: Int): String = {
    var out = s.substring(0, index) +"👻"+ s.substring(index+1)
    out
  }

  def checkUnique(path: Array[(Int, Int, Int, Int)], x: Int, y: Int): Boolean = {
    var res = false
    for(i <- 0 until path.length) {
      if(path(i)._1 == x && path(i)._2 == y) {
        res = true
      }
    }
    res
  }

  def calculateRoute(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Array[(Int,Int)] = {
    //Heuristic function f = cost + manhattan distance
    def diff(t: Array[(Int,Int,Int,Int)]) = t.last._3 + 10*t.last._4
    //A tuple is a node, the parameters are: x-coordinate y-coordinate cost manhattan distance
    var pq = new PriorityQueue[Array[(Int,Int,Int,Int)]]()(Ordering.by(diff))

    var res = Array((x_s,y_s,0,math.abs(x_s-x_e)+math.abs(y_s-y_e)))
    pq.enqueue(Array((x_s,y_s,0,math.abs(x_s-x_e)+math.abs(y_s-y_e))))
    var found = false
    var visited = Set((x_s,y_s))
    while(!pq.isEmpty && !found) {
      var path = pq.dequeue()
      var x = path.last._1
      var y = path.last._2
      var cost = path.last._3
      if(x==x_e && y==y_e) {
        res = path
        found = true
      }
      //x-1, y
      if(x-1>=0 && TownMap(x-1)(y)==0 && !visited.contains((x-1, y))&& !checkGhost(x-1, y)) {
          var path1 = path :+ (x -1,y,cost+1,math.abs((x-1)-x_e)+math.abs(y-y_e))
          pq.enqueue(path1)
          visited.+=((x-1, y))
      }
      //x+1, y
      if(x+1<=XB && TownMap(x+1)(y)==0 && !visited.contains((x+1, y)) && !checkGhost(x+1, y)) {
        var path2 = path :+ (x+1,y,cost+1,math.abs((x+1)-x_e)+math.abs(y-y_e))
        pq.enqueue(path2)
        visited.+=((x+1, y))
      }
      //x, y+1
      if(y+1<=YB && TownMap(x)(y+1)==0 && !visited.contains((x, y+1)) && !checkGhost(x, y+1)) {
        var path3 = path :+ (x,y+1,cost+1,math.abs(x-x_e)+math.abs((y+1)-y_e))
        pq.enqueue(path3)
        visited.+=((x, y+1))
      }
      //x, y-1
      if(y-1>0 && TownMap(x)(y-1)==0 && !visited.contains((x, y-1)) && !checkGhost(x, y-1)) {
        var path4 = path :+ (x,y-1,cost+1,math.abs(x-x_e)+math.abs((y-1)-y_e))
        pq.enqueue(path4)
        visited.+=((x, y-1))
      }
    }
    res.map(x => (x._1, x._2))
  }

  def getGhostLocation(): String = {
    var output = s"There are $ghostNumber ghosts in town and they are at:\n"
    var graph = printMap(ghosts, ghostNumber, Array())
    output+"\n"+graph
  }

  def printRoute(x1:Int, y1:Int, x2:Int, y2:Int): String = {
    var output = s"Output route from (${x1},${y1}) to (${x2},${y2})\n"
    var route = this.calculateRoute(x1,y1,x2,y2)
    var graph = printMap(ghosts, ghostNumber, route)
    output+"\n"+graph
  }

  def printMap(gs: Array[Ghost], gostNumber: Int, route: Array[(Int,Int)]): String = {
    var output = ""
      for( i <- 0 until MAP1.m.length) {
        for (j <- 0 until MAP1.m(1).length) {
          var include = false
          for(g <- 0 until gostNumber) {
            if(i ==gs(g).getX() && j == gs(g).getY()){
              if(!include) {
                include = true
                output = output + "👻"
              }
            }
            if ( i<=(gs(g).getX()+2) && i>=(gs(g).getX()-2) && j>=(gs(g).getY()-2) && j<=(gs(g).getY()+2)) {
              if(!include) {
                include = true
                output = output + "😱"
              }
            }
          }
          if (route.contains((i,j))) {
            output = output + "🔴"
          }else if (!include && 1 == MAP1.m(i)(j)) {
            output = output + "⬛"
          }else if (!include){
            output = output + "⬜"
          }
        }
        output = output+'\n'
      }
    output
  }


}


object Main extends App {
  var host = "0.0.0.0"
  var port = 3001
  var SIZE = 50
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
            pathRest match {
              case pos(s1, s2, e1, e2) => {
//                s"Start navigation for ($s1,$s2) and ($e1, $e2)"
                complete(town.printRoute(s1.toInt,s2.toInt,e1.toInt,e2.toInt))
              }
            }
//            complete(res)
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
