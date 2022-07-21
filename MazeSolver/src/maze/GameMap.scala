package maze

import play.api.libs.json.{JsValue, Json}
import week8.linkedlist.Queue


object GameMap {

  def findPath(start: GridLocation, end: GridLocation, map: List[List[MapTile]]): List[GridLocation] = {
    var ycount=0
    val Incord:List[List[(Int,Int)]]=(for(x<-map)yield{
      var xcount= -1
      val z:List[(Int,Int)]=(for(y<-x)yield{
        if(y.tileType=="wall"){
          xcount=xcount+1
          (0,0)
        }
        else{
          xcount=xcount+1
          (xcount,ycount)
        }
      })
      xcount=(-1)
      ycount=ycount+1
      z
    })
    var solution:List[GridLocation]=List()
    var connInt:List[((Int,Int),(Int,Int))]=List()
    var num1: Set[(Int,Int)] = Set((start.x,start.y))
    val toExplore: Queue[(Int,Int)] = new Queue()
    toExplore.enqueue(start.x,start.y)
    while (!toExplore.empty() && solution.isEmpty) {
      val nodeToExplore = toExplore.dequeue()
      for(n<-Incord){
        if((nodeToExplore._1,nodeToExplore._2)==(end.x,end.y)){
          solution=solution:+(end)
        }
        if(n.contains( (nodeToExplore._1+1,nodeToExplore._2) )) {
          if (!num1.contains(nodeToExplore._1+1,nodeToExplore._2)) {
            val newcord:(Int,Int)=(nodeToExplore._1+1,nodeToExplore._2)
            connInt = connInt :+ (nodeToExplore,newcord)
            //          println(connInt.toMap)
            toExplore.enqueue(newcord)
            num1 = num1 + newcord
          }
        }
        if(n.contains( (nodeToExplore._1,nodeToExplore._2+1) )) {
          if (!num1.contains(nodeToExplore._1,nodeToExplore._2+1)) {
            val newcord:(Int,Int)=(nodeToExplore._1,nodeToExplore._2+1)
            connInt = connInt :+ (nodeToExplore,newcord)
            //          println(connInt.toMap)
            toExplore.enqueue(newcord)
            num1 = num1 + newcord
          }
        }
        if(n.contains( (nodeToExplore._1-1,nodeToExplore._2) )) {
          if (!num1.contains(nodeToExplore._1-1,nodeToExplore._2)) {
            val newcord:(Int,Int)=(nodeToExplore._1-1,nodeToExplore._2)
            connInt = connInt :+ (nodeToExplore,newcord)
            //          println(connInt.toMap)
            toExplore.enqueue(newcord)
            num1 = num1 + newcord
          }
        }
        if(n.contains( (nodeToExplore._1,nodeToExplore._2-1) )) {
          if (!num1.contains(nodeToExplore._1,nodeToExplore._2-1)) {
            val newcord:(Int,Int)=(nodeToExplore._1,nodeToExplore._2-1)
            connInt = connInt :+ (nodeToExplore,newcord )
            //          println(connInt.toMap)
            toExplore.enqueue(newcord)
            num1 = num1 + newcord
          }
        }
      }
    }

    var Path:List[GridLocation]= List()
    var newPath:List[GridLocation]= List()
    def amo(end:GridLocation):(Int,Int)={
      var C:(Int,Int)=(0,0)
      for(x<-connInt){
        if((x._2)==(end.x,end.y)){
          val D:GridLocation= new GridLocation(x._1._1,x._1._2)
          C= amo(D)
          C=(D.x,D.y)
        }
      }
      val P:GridLocation= new GridLocation(C._1,C._2)
      Path=Path:+P
      C
    }
    amo(end)
    for(pp<-Path){
      if((pp.x,pp.y)!=(0,0)){
        newPath=newPath:+pp
      }
    }
    newPath=newPath:+end
    println(newPath)
    newPath
  }
  var Start = new GridLocation(1, 10)
  var End = new GridLocation(28, 10)
  var xxx: List[String] = List()

  def randList() {
    var cord:List[(Int,Int)]=List()
    var num1=2
    var num2=0
    for(x<-0 until 29){
      for(x<-0 until 13) {
        num2=num2+2
        cord = cord :+ (num1, num2)
      }
      num1=num1+2
      num2=0
    }
    cord=cord:+(29,2)
    cord=cord:+(29,5)
    cord=cord:+(29,8)
    cord=cord:+(29,11)
    cord=cord:+(29,15)
    cord=cord:+(29,17)

    println(cord)

//                                  (2,2),(2,3),(3,2),(3,3),
//                                  (5,2),(5,3),(6,2),(6,3),
//                                  (8,2),(8,3),(9,2),(9,3),
//                                  (11,2),(11,3),(12,2),(12,3),
//                                  (14,2),(14,3),(15,2),(15,3),
//                                  (17,2),(17,3),(18,2),(18,3),
//                                  (20,2),(20,3),(21,2),(21,3),
//                                  (23,2),(23,3),(24,2),(24,3),
//                                  (26,2),(26,3),(27,2),(27,3),
//
//                                  (2,5),(2,6),(3,5),(3,6),
//                                  (2,8),(2,9),(3,8),(3,9),
//                                  (2,11),(2,12),(3,11),(3,12),
//                                  (2,14),(2,15),(3,14),(3,15),
//                                  (2,17),(2,18),(3,17),(3,18),
//
//                                  (5,5),(5,6),(6,5),(6,6),
//                                  (5,8),(5,9),(6,8),(6,9),
//                                  (5,11),(5,12),(6,11),(6,12),
//                                  (5,14),(5,15),(6,14),(6,15),
//                                  (5,17),(5,18),(6,17),(6,18),
//
//                                  (8,5),(8,6),(9,5),(9,6),
//                                  (8,8),(8,9),(9,8),(9,9),
//                                  (8,11),(8,12),(9,11),(9,12),
//                                  (8,14),(8,15),(9,14),(9,15),
//                                  (8,17),(8,18),(9,17),(9,18),
//
//                                  (11,5),(11,6),(12,5),(12,6),
//                                  (11,8),(11,9),(12,8),(12,9),
//                                  (11,11),(11,12),(12,11),(12,12),
//                                  (11,14),(11,15),(12,14),(12,15),
//                                  (11,17),(11,18),(12,17),(12,18),
//
//                                  (14,5),(14,6),(15,5),(15,6),
//                                  (14,8),(14,9),(15,8),(15,9),
//                                  (14,11),(14,12),(15,11),(15,12),
//                                  (14,14),(14,15),(15,14),(15,15),
//                                  (14,17),(14,18),(15,17),(15,18),
//
//                                  (17,5),(17,6),(18,5),(18,6),
//                                  (17,8),(17,9),(18,8),(18,9),
//                                  (17,11),(17,12),(18,11),(18,12),
//                                  (17,14),(17,15),(18,14),(18,15),
//                                  (17,17),(17,18),(18,17),(18,18),
//
//                                  (20,5),(20,6),(21,5),(21,6),
//                                  (20,8),(20,9),(21,8),(21,9),
//                                  (20,11),(20,12),(21,11),(21,12),
//                                  (20,14),(20,15),(21,14),(21,15),
//                                  (20,17),(20,18),(21,17),(21,18),
//
//                                  (23,5),(23,6),(24,5),(24,6),
//                                  (23,8),(23,9),(24,8),(24,9),
//                                  (23,11),(23,12),(24,11),(24,12),
//                                  (23,14),(23,15),(24,14),(24,15),
//                                  (23,17),(23,18),(24,17),(24,18),
//
//                                  (26,5),(26,6),(27,5),(27,6),
//                                  (26,8),(26,9),(27,8),(27,9),
//                                  (26,11),(26,12),(27,11),(27,12),
//                                  (26,14),(26,15),(27,14),(27,15),
//                                  (26,17),(26,18),(27,17),(27,18)

    val r = scala.util.Random
    var x: List[List[Int]] = (for (x <- 0 until 20) yield {
      if (x == 0 || x == 19) {
        //   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
        List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      }
//      else if(x==1||x==3||x==5||x==7||x==9||x==11||x==13||x==15||x==18){
//        List(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
//      }
      else if(x==1||x==3||x==5||x==7||x==16||x==18){
        List(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
      }
      else if(x==11){
        List(0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0)
      }
      else if(x==9){
        List(0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0)
      }
      else if(x==13){
        List(0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
      }
      else if(x==15){
        List(0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0)
      }
      else if(x==2||x==17||x==12){
        List(0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0)
      }
      else {
        var y: List[Int] = List()
        for (x2 <- 0 until 30) {
          if (x2 == 0 || x2 == 29) {
            y = y :+ 0
          }
          else if(x==10 && x2==1){
            y=y:+1
          }
          else if(cord.contains(x,x2)){
            y=y:+0
          }
          else {
            y = y :+ r.nextInt(2)
          }
        }
        y
      }
    }).toList
    var c1=0
    var c2=0
    var graph: List[String] = (for (xx <- x) yield {
      c1=c1+1
      var Str: String = ""
      for (xxx <- xx) {
        c2=c2+1
        if((c1,c2)==(11,29)){
          Str = Str + "G"
        }
        else if (xxx == 0) {
          Str = Str + "O"
        }
        else {
          Str = Str + "-"
        }
      }
      c2=0
      Str
    }).toList
    xxx = graph
  }

  randList()
  println(xxx)
  var bool = true

  var fingraph: List[List[MapTile]]=List()
  while (bool) {
    val finalgraph: List[List[MapTile]] = (for (x <- xxx) yield {
      MapTile.generateRow(x)
    }).toList
    if (findPath(Start, End, finalgraph).contains(Start)&&
      findPath(Start, End, finalgraph).contains(End)) {
      fingraph=finalgraph
      bool=false
    }
    else{
      randList()
    }
  }





//  def apply(): GameMap = {
//    new GameMap {
//      tiles= List(
//        MapTile.generateRow("OOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"),
//        MapTile.generateRow("O----------------------------O"),
//        MapTile.generateRow("O-O-OOOO-OOO-O-O-O-OOOOOOOOO-O"),
//        MapTile.generateRow("O-O----------O---O-O---------O"),
//        MapTile.generateRow("O-OOOOOOOOOOOOOO-O-OOOOOOOOOOO"),
//        MapTile.generateRow("O-O-O----------OOO--O---OO---O"),
//        MapTile.generateRow("O-O---OO-OOOOOOO-OO-O-O-OO-O-O"),
//        MapTile.generateRow("O-OOOOO--O-----O--O---O-OO-O-O"),
//        MapTile.generateRow("O-------OO-OOO-OO-OOOOO----O-O"),
//        MapTile.generateRow("OOO-OOOOO--O-O-OO-OO-OOOOOOO-O"),
//        MapTile.generateRow("O--------------------------OGO"),
//        MapTile.generateRow("OO-OOOOOOOOOOOOOOOOOOOOOOOOO-O"),
//        MapTile.generateRow("O--OO---O---O---O---O---O--O-O"),
//        MapTile.generateRow("O-OO--O-O-O-O-O-O-O-O-O-O----O"),
//        MapTile.generateRow("O----OO-O-O---O---O---O---OOOO"),
//        MapTile.generateRow("OOOOOO--O-OOOOOOOOOOOOOOOOOOOO"),
//        MapTile.generateRow("O------OO--------------------O"),
//        MapTile.generateRow("O-OOOOOOOOOOOOOOOOOOOOOOOOOO-O"),
//        MapTile.generateRow("O----------------------------O"),
//        MapTile.generateRow("OOOOOOOOOOOOOOOOOOOOOOOOOOOOOO")
//      )
//      startingLocation = new GridLocation(1, 10)
//    }
//  }
def apply(): GameMap = {
  new GameMap {
    tiles= fingraph
    startingLocation = new GridLocation(1, 10)
  }
}


}

class GameMap {
  var tiles: List[List[MapTile]] = List()
  var startingLocation = new GridLocation(0, 0)

  def tilesJSON(): JsValue = {
    Json.toJson(tiles.map((row: List[MapTile]) => row.map((tile: MapTile) => Json.toJson(
      Map("type" -> Json.toJson(tile.tileType), "passable" -> Json.toJson(tile.passable))
    ))))
  }

}
