package solver

import maze.{GridLocation, MapTile, PhysicsVector}
import week8.linkedlist. Queue


object PathFinding {

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



  def getVelocity(path: List[GridLocation], currentLocation: PhysicsVector): PhysicsVector = {

    def move(input: PhysicsVector): PhysicsVector = {
      val times5 = new PhysicsVector(input.x *5, input.y *5)
      times5
    }

    val gridLoc = new GridLocation(currentLocation.x.toInt, currentLocation.y.toInt)
    var count = false
    var nextValue = new GridLocation(0,0)
    var endValue = new PhysicsVector(0,0)

    for (x <- path) {
      if (x == gridLoc) {
        count = true
      }
      else if (count) {
        nextValue = x
        count = false
      }
    }

    val center= new PhysicsVector(currentLocation.x.floor+0.5,currentLocation.y.floor+0.5)
    val nextValue2= new PhysicsVector(nextValue.x+0.5,nextValue.y+0.5)
//    println(center)
//    println(nextValue2)
    if(path.nonEmpty){
      if(nextValue.x==0 && nextValue.y==0){
        val Vector= new PhysicsVector(center.x-currentLocation.x,center.y-currentLocation.y).normal2d()
//        println(Vector)
        if(currentLocation.distance2d(center)>0.1) {
          println(currentLocation.distance2d(center))
          endValue = move(Vector)
        }
      }
      else{
        val Vector = new PhysicsVector(nextValue2.x-currentLocation.x,nextValue2.y-currentLocation.y).normal2d()
        endValue=move(Vector)
      }
    }
//    println(endValue)
    endValue
//     1,0 to the east  next x is greater than this x
//     0,1 goes south   next y is greater than this y
//     -1,0 goes west   next x is less than this x
//     0,-1 goes north  next y is less than this y

  }
}
