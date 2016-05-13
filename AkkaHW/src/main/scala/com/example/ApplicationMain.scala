package com.example

import akka.actor.{ActorRef, Props, ActorSystem}

object ApplicationMain {


  def main(args: Array[String]) {

val system = ActorSystem("MyActorSystem")

    val matActor : Array[Array[ActorRef]] = Array.ofDim[ActorRef](2,2)

    for (i <- 0 to 1;
         j <- 0 to 1
      )    matActor(i)(j) =  system.actorOf(Props(classOf[MatActor], i, j))

    //(  system.actorOf(Props[MatActor], "pingActor")


    matActor(0)(0) ! MatActor.RowElement(3)
    matActor(0)(0) ! MatActor.RowElement(5)
    // This example app will ping pong 3 times and thereafter terminate the ActorSystem -
    // see counter logic in PingActor
    //system.awaitTermination()
  }
}