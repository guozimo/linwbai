package com.bai.web

import javax.servlet.http.HttpSession

import com.bai.model.{Clean, MinMaxAI, Play, Pos}

import scala.collection._
import org.springframework.web.bind.annotation.{RequestMapping, RestController}

/**
  * Created by linwbai on 16-10-8.
  */
@RestController
@RequestMapping(Array("/web"))
class RestfulController {

  var map:concurrent.Map[String,MinMaxAI] = new concurrent.TrieMap
  //val system = ActorSystem("baiActor")
  //val baiActor = system.actorOf(Props[BaiActor])

  @RequestMapping(Array("begin"))
  def begin(session: HttpSession): Pos = {
    val sessionId = session.getId
    if(!map.contains(sessionId)){
      map += (sessionId -> new MinMaxAI)
    }
    val ai = map(sessionId)
    val pos = new Pos
    pos.col = 7
    pos.row = 7
    pos.value = 2
    ai.play(pos)
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
      map -= sessionId
      return play
    }
    play.pos = ai.getPlay
    ai.play(play.pos)
    if (ai.over){
      map -= sessionId
      play.win = "还没有AI聪明,行不行啊"
    }
    play
  }

  @RequestMapping(Array("/clean"))
  def clean(session: HttpSession) : String = {
    //baiActor ! Clean(map)
    if (map.contains(session.getId)){
      map -= session.getId
    }
    "ok"
  }
}
