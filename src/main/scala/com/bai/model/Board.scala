package com.bai.model

/**
  * Created by linwbai on 16-10-11.
  */
class Board {

  val dx = Array(1,0,1,1)
  val dy = Array(0,1,1,-1)

  var step: Int = _ //步数
  var cell: Array[Array[Cell]] = _
  var cand: Array[Pos] = _
  var stack: Array[Pos] = _
  var bestMove: Pos = _
  var isLose: Array[Boolean] = _
  var patternTable: Array[Array[Int]] = _

  def play(pos: Pos): Unit ={
    step += 1
    cell(pos.row)(pos.col).piece = step & 1
    stack(step) = pos

  }

  def delPlay(): Unit ={
    val x = stack(step).row
    val y = stack(step).col
    step -= 1
    cell(x)(y).piece = 0
    updateCell(x,y)
  }

  def updateCell(x: Int, y: Int): Unit ={
    var a,b,key = 0
    for(i <- 0 until 4) {
      a = x + dx(i)
      b = y + dy(i)
      for (j <- 0 until 4){
        a += dx(i)
        b += dy(i)
        if (a < 15 && b < 15) {
          key = getKey(x,y,i)
          cell(a)(b).pattern(0)(i) = patternTable(key)(0)
          cell(a)(b).pattern(1)(i) = patternTable(key)(1)
        }
      }
      a = x - dx(i)
      b = y - dy(i)
      for (k <- 0 until 4){
        a -= dx(i)
        b -= dy(i)
        if (a < 15 && b < 15) {
          key = getKey(x,y,i)
          cell(a)(b).pattern(0)(i) = patternTable(key)(0)
          cell(a)(b).pattern(1)(i) = patternTable(key)(1)
        }
      }
    }
  }

  def getKey(x: Int,y: Int,i: Int): Int ={
    var key = 0
    var a = x - dx(i)*4
    var b = y - dy(i)*4
    for (k <- 0 until 9){
      a += dx(i)
      b += dy(i)
      if (k != 4) {
        key <<= 2
        key ^= cell(a)(b).piece
      }
    }
    key
  }

  def checkWin(): Boolean ={
    val color = step & 1
    val c = cell(stack(step).row)(stack(step).col)
    c.pattern(color)(0) == 7 || c.pattern(color)(1) == 7 || c.pattern(color)(2) == 7 || c.pattern(color)(3) == 7
  }
}
