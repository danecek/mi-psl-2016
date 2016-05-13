package com.example

import akka.actor.{ActorRef, Actor}

import scala.collection.mutable.ArrayBuffer


class MatActor(val row : Int, col : Int) extends Actor {

  var rn: ActorRef = _
  var cn: ActorRef = _

  val colBuf = new ArrayBuffer[Int]
  val rowBuf = new ArrayBuffer[Int]

  import MatActor._
  def receive = {
    case RowElement(value) => {
      rowBuf += value
      println(rowBuf)
      if (rn != null) rn ! RowElement(value)
    }
    case ColElement(value) => {
      colBuf += value
      if (colBuf != null) cn ! RowElement(value)
      println(colBuf)
    }
  }
}
 object MatActor {

  case class RowElement(value: Int)

  case class ColElement(value: Int)

}
