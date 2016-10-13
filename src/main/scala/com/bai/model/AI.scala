package com.bai.model

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks

/**
  * Created by linwbai on 16-10-11.
  */
class AI {

  val meVal = Array(0,3,7,7,18,18)

  val youVal = Array(0,2,5,5,12,12)

  val cVal = Array(0, 3, 18, 27, 144, 216, 1200, 1800)
  val hVal = Array(0, 2, 12, 18, 96, 144, 800, 1200)

  val searchDepth = 12

  var total = 0

  var bestValue = 0

  val board = new Board

  initType()
  initPattern()
  initCell()
  board.isLose = Array.fill[Boolean](51)(false)
  board.stack = Array.fill[Pos](400)(new Pos)

  def initCell(): Unit ={
    board.b_start = 4
    board.b_end = board.size + 4
    for (i <- 0 until 28) {
      for (j <- 0 until 28) {
        board.cell(i)(j) = new Cell
        if (i < 4 || i >= 15 + 4 || j < 4 || j >= 15 + 4)
          board.cell(i)(j).piece = board.outside
        else
          board.cell(i)(j).piece = board.empty
      }
    }
  }

  def initPattern(): Unit ={
    for (i <- 0 until 65536) {
      board.patternTable(i)(0) = lineType(0,i)
      board.patternTable(i)(1) = lineType(1,i)
    }
    board.patternTable.foreach(i => i.foreach(j => println(j)))
  }

  def initType(): Unit ={
    for (i <- 1 until 10){
      for (j <- 1 until 6){
        for (k <- 1 until 6){
          for (l <- 1 until 3){
            board.typeTable(i)(j)(k)(l) = board.getType(i,j,k,l)
          }
        }
      }
    }
  }

  def lineType(color: Int, key: Int): Int ={
    val line_left = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1)
    val line_right = Array(-1,-1,-1,-1,-1,-1,-1,-1,-1)
    var keyC = key
    for (i <- 0 until 9){
      if (i == 4) {
        line_left(i) = color
        line_right(i) = color
      } else {
        line_left(i) = key & 3
        line_right(8 - i) = key & 3
        keyC >>= 2
      }
    }
    val p1 = shortLine(line_left)
    val p2 = shortLine(line_right)
    if(p1 == board.block3 && p2 == board.block3)
      return board.checkFlex3(line_left)
    if (p1 == board.block4 && p2 == board.block4)
      return board.checkFlex4(line_left)
    if (p1 > p2)
      return p1
    p2
  }

  def shortLine(line: Array[Int]): Int ={
    var kong,block = 0
    var len,len2,count = 1
    val color = line(4)
    val loop = new Breaks
    loop.breakable{
      for (k <- 5 to 8) {
        if (line(k) == color) {
          if (kong + count > 4)
            loop.break()
          count += 1
          len += 1
          len2 = kong + count
        } else if (line(k) == board.empty){
          len += 1
          kong += 1
        } else {
          block += 1
          loop.break()
        }
      }
    }

    kong = len2 - count
    loop.breakable {
      for (k <- Range(3,0,-1)) {
        if (line(k) == color) {
          if (kong + count > 4)
            loop.break()
          count += 1
          len += 1
          len2 = kong + count
        } else if (line(k) == board.empty) {
          len += 1
          kong += 1
        } else {
          if (len2 == kong + block) {
            block += 1
            loop.break()
          }
        }
      }
    }
    board.typeTable(len)(len2)(count)(block)
  }


  def getBest(): Pos ={
    stopThink = false
    total = 0
    board.bestMove.value = 1
    if (board.step == 0){
      board.bestMove.row = 7
      board.bestMove.col = 7
      return board.bestMove
    }
    if (board.step == 1 || board.step == 2){
      var x,y = 0
      val random = new Random()
      do {
        x = random.nextInt(board.step*2 + 1) + board.stack(1).row - board.step
        y = random.nextInt(board.step*2 + 1) + board.stack(1).col - board.step
      } while (board.cell(x)(y).piece != 2 || x >= 15 || x <= 0 || y >= 15 || y <= 0)
      board.bestMove.row = x
      board.bestMove.col = y
      return board.bestMove
    }

    board.isLose = Array.fill[Boolean](51)(false)
    for (i <- Range(0,searchDepth,2)) {
      if (i < 10) {
        maxDepth = i
        bestValue = search(i, -10001, 10000)
        if (bestValue == 10000) {
          return board.bestMove
        }
      }
    }
    board.bestMove
  }

  def search(depth: Int, alpha: Int, beta: Int): Int ={
    var alphaR = alpha
    val moves = ArrayBuffer[Pos]()
    val count = getMoves(moves,27)
    if (count == 1) {
      board.bestMove = moves(1)
      return 0
    }
    moves(0) = board.bestMove
    var value = 0
    for (i <- 0 to count) {
      if ((i > 0 && board.bestMove.equalsVal(moves(1))) || board.isLose(i)){

      }else{
        board.play(moves(i))
        while (true) {
          if (i > 1 && alpha + 1 < beta) {
            value = -AlphaBeta(depth - 1, -alpha - 1, -alpha)
            if (value <= alpha || value >= beta){

            }else {
              value = -AlphaBeta(depth - 1, -beta, -alpha)
            }
          }
        }
        board.delPlay()

        if(!stopThink){
          if (value == -10000)
            board.isLose(i) = true
          if (value >= beta) {
            board.bestMove = moves(i)
            return value
          }
          if (value > alpha) {
            alphaR = value
            board.bestMove = moves(i)
          }
        }
      }
    }
    if (alphaR == -10001){
      return bestValue
    }
    alphaR
  }

  var cnt = 1000
  var stopThink = false
  var maxDepth = 0

  def AlphaBeta(depth: Int, alpha: Int, beta: Int): Int ={
    var alphaR = alpha
    total += 1
    cnt -= 1
    if(cnt <= 0){
      cnt = 1000
      if (maxDepth > 4)
        stopThink = true
      if (board.checkWin())
        return -10000
      if (depth == 0)
        return evaluate()
    }
    val moves: ArrayBuffer[Pos] = ArrayBuffer()
    val count = getMoves(moves,27)
    var value = 0
    for (i <- 0 until count) {
      board.play(moves(i))
      while (true) {
        if (i > 1 && alpha + 1 < beta) {
          value = -AlphaBeta(depth - 1, -alpha - 1, -alpha)
          if (value <= alpha || value >= beta){

          }else {
            value = -AlphaBeta(depth - 1, -beta, -alpha)
          }
        }
      }
      board.delPlay()

      if (!stopThink){
        if (value >= beta){
          return value
        }
        if (value > alpha){
          alphaR = value
        }
      }
    }
    alphaR
  }

  def evaluate(): Int ={
    val cType = ArrayBuffer[Int](0,0,0,0,0,0,0,0)
    val hType = ArrayBuffer[Int](0,0,0,0,0,0,0,0)
    var cScore,hScore = 0
    val me = (board.step + 1) & 1
    val you = board.step & 1
    var cell: Cell = new Cell
    for (i <- 5 until 19;j <- 5 until 19){
      if(board.cell(i)(j).piece == 2){
        cell = board.cell(i)(j)
        typeCount(cell,me,cType)
        typeCount(cell,you,hType)

      }
    }

    if (cType(board.win) > 0)
      return 10000
    if (hType(board.win) > 1)
      return -10000
    if (cType(board.flex4) > 0 && hType(board.win) == 0)
      return 10000

    for (i <- 1 until 8) {
      cScore += cType(i)*cVal(i)
      hScore += hType(i)*hVal(i)
    }
    cScore - hScore
  }

  def updateRound(n: Int): Unit ={

  }

  def getMoves(moves: ArrayBuffer[Pos], branch: Int): Int = {
    var cSize,mSize: Int = 0
    var score = 0
    for (i <- 0 until 15){
      for(j <- 0 until 15){
        if(board.cell(i)(j).piece == board.empty){
          score = getScore(i,j)
          if(score > 0){
            cSize += 1
            board.cand(cSize).row = i
            board.cand(cSize).col = j
            board.cand(cSize).score = score
            board.cand.sortBy(p => p.score)
          }
        }
      }
    }
    if (cSize > branch)
      cSize = branch
    mSize = cutCand(moves,cSize)
    if (mSize == 0){
      mSize = cSize
      for (i <- 2 to mSize){
        moves += board.cand(i)
      }
    }
    moves.length
  }

  def getScore(x: Int, y: Int): Int ={
    val meType = ArrayBuffer[Int](0,0,0,0,0,0,0,0)
    val youType = ArrayBuffer[Int](0,0,0,0,0,0,0,0)
    val c = board.cell(x)(y)

    typeCount(c,(board.step + 1) & 1,meType)
    typeCount(c,board.step & 1,youType)
    if (meType(board.win) > 0)
      return 10000
    if (youType(board.win) > 0)
      return 5000
    if (meType(board.flex4) > 0 || meType(board.block4) > 1)
      return 2400
    if (meType(board.block4) > 0 && meType(board.flex3) > 0)
      return 2000
    if (youType(board.flex4) > 0 || youType(board.block4) > 1)
      return 1200
    if (youType(board.block4) > 0 && youType(board.flex3) > 0)
      return 1000
    if (meType(board.flex3) > 1)
      return 400
    if (youType(board.flex3) > 1)
      return 200
    var score = 0
    for (i <- 0 to board.block4){
      score += meType(i)*meVal(i)
      score += youType(i)*youVal(i)
    }
    score
  }

  def cutCand(moves: ArrayBuffer[Pos], cSize: Int): Int = {
    val me = (board.step + 1) & 1
    val you = board.step & 1
    var mSize = 0
    if (board.cand(1).score >= 2400){
      mSize += 1
      moves += board.cand(1)
    } else if (board.cand(1).score == 1200) {
      mSize += 1
      moves += board.cand(1)
      if (board.cand(2).score == 1200) {
        mSize += 1
        moves += board.cand(2)
      }

      for (i <- mSize + 2 to cSize) {
        if (isType(i,board.block4,me) || isType(i,board.block4,you)){
          mSize += mSize
          moves(mSize) = board.cand(i)
        }
      }
    }
    mSize
  }

  def isType(i: Int, types: Int,color: Int): Boolean = {
    board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(0) == types || board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(1) == types || board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(2) == types || board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(3) == types
  }

  def typeCount(cell: Cell, color: Int, meType: ArrayBuffer[Int]): Unit={
    meType(cell.pattern(color)(0)) += 1
    meType(cell.pattern(color)(1)) += 1
    meType(cell.pattern(color)(2)) += 1
    meType(cell.pattern(color)(3)) += 1
//    cell.pattern(color).foreach(i => meType(i) = meType(i) + 1)
  }

}
