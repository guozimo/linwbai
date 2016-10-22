package com.bai.utils

import akka.actor._
import scala.collection.concurrent
import com.bai.model.{Clean, MinMaxAI}

/**
  * Created by linwbai on 16-10-18.
  */
class BaiActor extends Actor {


  override def receive: Receive = {
    case Clean(map) => sender ! clean(map)
  }

  def clean(map: concurrent.Map[String, MinMaxAI]): String = {
    map.foreach(kv => {
      if (System.currentTimeMillis() - kv._2.start_time > 360){
        map -= kv._1
      }
    })
    "ok"
  }
}


