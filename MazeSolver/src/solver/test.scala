package solver

import week8.linkedlist.Queue
import maze.{GridLocation, MapTile}
import solver.PathFinding
object test {
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
  def main(arg: Array[String]): Unit = {
    var Start = new GridLocation(1, 10)
    var End = new GridLocation(28, 10)
    var xxx: List[String] = List()

    def randList() {
      val r = scala.util.Random
      var x: List[List[Int]] = (for (x <- 0 until 20) yield {
        if (x == 0 || x == 19) {
          //   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
          List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        }
        else {
          var y: List[Int] = List()
          for (x <- 0 until 30) {
            if (x == 0 || x == 29) {
              y = y :+ 0
            }
            else {
              y = y :+ r.nextInt(2)
            }
          }
          y
        }
      }).toList
      var graph: List[String] = (for (xx <- x) yield {
        var Str: String = ""
        for (xxx <- xx) {
          if (xxx == 0) {
            Str = Str + "O"
          }
          else {
            Str = Str + "-"
          }
        }
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

      if (findPath(Start, End, finalgraph).contains(End)) {
        fingraph=finalgraph
        bool=false
      }
      else{
        randList()
      }
    }
    println(fingraph)
  }

}
