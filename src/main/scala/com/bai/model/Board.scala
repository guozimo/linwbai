package com.bai.model

/**
  * Created by linwbai on 16-10-11.
  */
class Board {

  val win = 7; // 连五

  val flex4 = 6; // 活四

  val block4 = 5; // 冲四

  val flex3 = 4; // 活三

  val block3 = 3; // 眠三

  val flex2 = 2; // 活二

  val block2 = 1; // 眠二

  val nType = 8
  val maxSize = 20
  val dx = Array(1,0,1,1)
  val dy = Array(0,1,1,-1)

  val outside = 3
  val empty = 2
  val black = 1
  val white = 0

  var step: Int = 0 //步数
  val size = 15
  var b_start,b_end = 0
  var cell: Array[Array[Cell]] = Array.ofDim(28,28)
  var cand: Array[Pos] = Array.fill(256)(new Pos)
  var stack: Array[Pos] = Array.fill(400)(new Pos)
  var bestMove: Pos = new Pos
  var isLose: Array[Boolean] = Array.fill(51)(false)
  var patternTable: Array[Array[Int]] = Array.ofDim(65536,2)
  var typeTable: Array[Array[Array[Array[Int]]]] = Array.ofDim(10,6,6,3)

  def play(pos: Pos): Unit ={
    step += 1
    cell(pos.row)(pos.col).piece = step & 1
    stack(step) = pos
    updateCell(pos.row, pos.col)
  }

  def delPlay(): Unit ={
    val x = stack(step).row
    val y = stack(step).col
    step -= 1
    cell(x)(y).piece = empty
    updateCell(x,y)
  }

  def updateCell(x: Int, y: Int): Unit ={
    var a,b,key = 0
    for(i <- 1 until 4) {
      a = x + dx(i)
      b = y + dy(i)
      for (j <- 1 until 4){
        if (a < 15 && b < 15) {
          key = getKey(x,y,i)
          cell(a)(b).pattern(0)(i) = patternTable(key)(0)
          cell(a)(b).pattern(1)(i) = patternTable(key)(1)
        }
        a += dx(i)
        b += dy(i)
      }
      a = x - dx(i)
      b = y - dy(i)
      for (k <- 1 until 4){
        if (a < 15 && b < 15) {
          key = getKey(x,y,i)
          cell(a)(b).pattern(0)(i) = patternTable(key)(0)
          cell(a)(b).pattern(1)(i) = patternTable(key)(1)
        }
        a -= dx(i)
        b -= dy(i)
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

  def checkFlex3(line: Array[Int]): Int = {
    val color = line(4)
    var types = 0
    for (i <- 0 until 9) {
      if (line(i) == empty) {
        line(i) = color
        types = checkFlex4(line)
        line(i) = empty
        if (types == 6)
          return 4
      }
    }
    3
  }

  def checkFlex4(line: Array[Int]): Int = {
    var j,count,five = 0
    val color = line(4)
    for (i <- 0 until 9) {
      if (line(i) == empty)
        count = 0
      j = i - 1
      while(j >= 0 && line(j) == color) {
        count += 1
        j -= 1
      }
      j = i + 1
      while (j <= 8 && line(j) == color) {
        count += 1
        j += 1
        if (count >= 4)
          five += 1
      }
    }
    if (five >= 2)
      return 6
    5
  }

  def getType(len: Int,len2: Int, count: Int, block: Int): Int = {
    if (len >= 5 && count > 1) {
      if (count == 5)
        return 7
      if (len > 5 && len2 < 5 && block == 0) {
        count match {
          case 2 => return 2
          case 3 => return 4
          case 4 => return 6
        }
      } else {
        count match {
          case 2 => return 1
          case 3 => return 3
          case 4 => return 5
        }
      }
    }
    0
  }
}
