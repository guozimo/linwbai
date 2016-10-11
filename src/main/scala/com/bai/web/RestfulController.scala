package com.bai.web

import javax.servlet.http.HttpSession

import org.springframework.web.bind.annotation.{RequestMapping, RestController}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by linwbai on 16-10-8.
  */
@RestController
@RequestMapping(Array("/web"))
class RestfulController {

  var map:Map[String,Array[Array[Int]]] = Map()
  var stackMap: Map[String,ArrayBuffer[Map[Int,Int]]] = Map()

  @RequestMapping(Array("/judgment"))
  def judgment(x :Int, y :Int, value : Int, session :HttpSession) :String = {
    val sessionId = session.getId
    if(!map.contains(sessionId)){
      map+=(sessionId -> Array.ofDim[Int](15,15))
      stackMap+=(sessionId -> ArrayBuffer[Map[Int,Int]]())
    }
    stackMap(sessionId) += Map(x -> y)
    judge(x,y,value,sessionId)
  }

  @RequestMapping(Array("/clean"))
  def clean(session: HttpSession) : String = {
    if (map.contains(session.getId)){
      map -= session.getId
    }
    "ok"
  }

  def judge(x :Int, y :Int, value : Int, key :String) : String = {
    val array  = map(key)
    array(x)(y) = value

    def foreachO: Int = {
      var x_c :Int = x
      var count = 0
      while (array(x_c)(y) == value && x_c < 15){
        x_c += 1
        count +=1
      }
      x_c = x
      while (array(x_c)(y) == value && x_c > 0){
        x_c -= 1
        count +=1
      }
      count
    }
    def foreachT: Int = {
      var y_c: Int = y
      var count = 0
      while (array(x)(y_c) == value && y_c < 15){
        y_c += 1
        count +=1
      }
      y_c = y
      while (array(x)(y_c) == value && y_c > 0){
        y_c -= 1
        count +=1
      }
      count
    }

    def foreachTr: Int = {
      var x_c :Int = x
      var y_c: Int = y
      var count = 0
      while (array(x_c)(y_c) == value && y_c < 15 && x_c < 15){
        y_c += 1
        x_c += 1
        count +=1
      }
      x_c = x
      y_c = y
      while (array(x_c)(y_c) == value && y_c > 0 && x_c > 0){
        x_c -= 1
        y_c -= 1
        count +=1
      }
      count
    }

    def foreachF: Int = {
      var x_c :Int = x
      var y_c: Int = y
      var count = 0
      while (array(x_c)(y_c) == value && y_c < 15 && x_c > 0){
        y_c += 1
        x_c -= 1
        count +=1
      }
      x_c = x
      y_c = y
      while (array(x_c)(y_c) == value && y_c > 0 && x_c < 15){
        x_c += 1
        y_c -= 1
        count +=1
      }
      count
    }

    if(foreachO >= 6 || foreachT >= 6 || foreachTr >= 6 || foreachF >= 6) {
      return "win"
    }
    "play"
  }
}
