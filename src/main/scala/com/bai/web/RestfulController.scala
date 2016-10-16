package com.bai.web

import javax.servlet.http.HttpSession

import com.bai.model.{AI, MinMaxAI, Play, Pos}
import org.springframework.web.bind.annotation.{RequestMapping, RestController}

/**
  * Created by linwbai on 16-10-8.
  */
@RestController
@RequestMapping(Array("/web"))
class RestfulController {

  var map:Map[String,MinMaxAI] = Map()

  @RequestMapping(Array("begin"))
  def begin(session: HttpSession): Pos = {
    val sessionId = session.getId
    if(!map.contains(sessionId)){
//      map+=(sessionId -> new AI)
      map += (sessionId -> new MinMaxAI)
    }
    val ai = map(sessionId)
    val pos = new Pos
    pos.col = 7
    pos.row = 7
    pos.value = 2
    ai.play(pos)
//    val pos = ai.getBest
//    ai.board.play(pos)
    pos
  }

  @RequestMapping(Array("/play"))
  def judgment(x :Int, y :Int, value : Int, session :HttpSession): Play = {
    val sessionId = session.getId
    val ai = map(sessionId)
    val pos = new Pos
    val play = new Play
    pos.row = x
    pos.col = y
    pos.value = value
    ai.play(pos)
    if (ai.over) {
      play.win = "你已经比AI更聪明了"
      return play
    }
    play.pos = ai.getPlay
    ai.play(play.pos)
    if (ai.over){
      play.win = "还没有AI聪明,行不行啊"
    }
    //    ai.board.play(pos)
//    if (ai.board.checkWin()){
//      play.win = "你已经比AI更聪明了"
//      return play
//    }
//    val best = ai.getBest
//    ai.board.play(best)
//    play.pos = best
//    if (ai.board.checkWin()){
//      play.win = "还没有AI聪明,行不行啊"
//      return play
//    }
    play
  }

  @RequestMapping(Array("/clean"))
  def clean(session: HttpSession) : String = {
    if (map.contains(session.getId)){
      map -= session.getId
    }
    "ok"
  }

//  def judge(x :Int, y :Int, value : Int, key :String) : String = {
//    val array  = map(key)
//    array(x)(y) = value
//
//    def foreachO: Int = {
//      var x_c :Int = x
//      var count = 0
//      while (array(x_c)(y) == value && x_c < 15){
//        x_c += 1
//        count +=1
//      }
//      x_c = x
//      while (array(x_c)(y) == value && x_c > 0){
//        x_c -= 1
//        count +=1
//      }
//      count
//    }
//    def foreachT: Int = {
//      var y_c: Int = y
//      var count = 0
//      while (array(x)(y_c) == value && y_c < 15){
//        y_c += 1
//        count +=1
//      }
//      y_c = y
//      while (array(x)(y_c) == value && y_c > 0){
//        y_c -= 1
//        count +=1
//      }
//      count
//    }
//
//    def foreachTr: Int = {
//      var x_c :Int = x
//      var y_c: Int = y
//      var count = 0
//      while (array(x_c)(y_c) == value && y_c < 15 && x_c < 15){
//        y_c += 1
//        x_c += 1
//        count +=1
//      }
//      x_c = x
//      y_c = y
//      while (array(x_c)(y_c) == value && y_c > 0 && x_c > 0){
//        x_c -= 1
//        y_c -= 1
//        count +=1
//      }
//      count
//    }
//
//    def foreachF: Int = {
//      var x_c :Int = x
//      var y_c: Int = y
//      var count = 0
//      while (array(x_c)(y_c) == value && y_c < 15 && x_c > 0){
//        y_c += 1
//        x_c -= 1
//        count +=1
//      }
//      x_c = x
//      y_c = y
//      while (array(x_c)(y_c) == value && y_c > 0 && x_c < 15){
//        x_c += 1
//        y_c -= 1
//        count +=1
//      }
//      count
//    }
//
//    if(foreachO >= 6 || foreachT >= 6 || foreachTr >= 6 || foreachF >= 6) {
//      return "win"
//    }
//    "play"
//  }
}
