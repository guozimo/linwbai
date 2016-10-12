package com.bai.model

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by linwbai on 16-10-11.
  */
class AI {

  val win = 7; // 连五

  val flex4 = 6; // 活四

  val block4 = 5; // 冲四

  val flex3 = 4; // 活三

  val block3 = 3; // 眠三

  val flex2 = 2; // 活二

  val block2 = 1; // 眠二

  val meVal = Array(0,3,7,7,18,18)

  val youVal = Array(0,2,5,5,12,12)

  val cVal = Array(0, 3, 18, 27, 144, 216, 1200, 1800)
  val hVal = Array(0, 2, 12, 18, 96, 144, 800, 1200)

  val searchDepth = 12

  var total = 0

  def getBest(board: Board): Unit ={
    board.bestMove.value = 1
    if (board.step == 0){
      board.bestMove.row = 7
      board.bestMove.col = 7
    }
    if (board.step == 1 || board.step == 3){
      var x,y = 0
      val random = new Random()
      do {
        x = random.nextInt(board.step*2 + 1) + board.stack(1).row - board.step
        x = random.nextInt(board.step*2 + 1) + board.stack(1).col - board.step
      } while (board.cell(x)(y).piece != 0 || x > 15 || x < 0 || y > 15 || y < 0)
      board.bestMove.row = x
      board.bestMove.col = y
    }

    var i = 0
    while (i < searchDepth){
      i += 2

    }
  }

  def search(depth: Int, alpha: Int, beta: Int, board: Board): Unit ={
    val moves: Array[Pos] = _
    val count = getMoves(moves,27,board)
    if (count == 1) {
      board.bestMove.row = moves(1).row
      board.bestMove.col = moves(1).col
      board.bestMove.score = moves(1).score
      return
    }
    moves(0) = board.bestMove

    for (i <- 0 to count) {
      if ((i > 0 && board.bestMove.equalsVal(moves(1))) || board.isLose(i)){

      }else{
        board.play(moves(i))
      }
    }
  }

  var cnt = 1000
  var stopThink = false
  var maxDepth = 0

//  def AlphaBeta(depth: Int, alpha: Int, beta: Int, board: Board): Int ={
//    total += 1
//    cnt -= 1
//    if(cnt <= 0){
//      cnt = 1000
//      if (maxDepth > 4)
//        stopThink = true
//      if (board.checkWin())
//        return -10000
//      if (depth == 0)
//        return
//    }
//
//  }

  def evaluate(board: Board): Int ={
    var cType = ArrayBuffer[Int]()
    var hType = ArrayBuffer[Int]()
    var cScore,hScore = 0
    val me = (board.step + 1) & 1
    val you = board.step & 1
    var cell: Cell = new Cell
    for (i <- 0 until 15;j <- 0 until 15){
      if(board.cell(i)(j).piece == 0){
        cell = board.cell(i)(j)
        typeCount(cell,me,cType)
        typeCount(cell,you,hType)

      }
    }

    if (cType(win) > 0)
      return 10000
    if (hType(win) > 1)
      return -10000
    if (cType(flex4) > 0 && hType(win) == 0)
      return 10000

    for (i <- 0 until 8) {
      cScore += cType(i)*cVal(i)
      hScore += hType(i)*hVal(i)
    }
    cScore - hScore
  }

  def updateRound(n: Int): Unit ={

  }

  def getMoves(moves: Array[Pos], branch: Int, board: Board): Int = {
    var cSize,mSize: Int = 0
    var score = 0
    for (i <- 0 until 15){
      for(j <- 0 until 15){
        if(board.cell(i)(j).piece == 0){
          score = getScore(i,j,board)
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
    mSize = cutCand(moves, board,cSize)
    if (mSize == 0){
      mSize = cSize
      for (i <- 0 to mSize){
        moves(i) = board.cand(i)
      }
    }
    moves.length
  }

  def getScore(x: Int, y: Int, board: Board): Int ={
    val meType = ArrayBuffer[Int]()
    val youType = ArrayBuffer[Int]()
    val c = board.cell(x)(y)
    typeCount(c,(board.step + 1) & 1,meType)
    typeCount(c,board.step & 1,youType)
    if (meType(win) > 0)
      return 10000
    if (youType(win) > 0)
      return 5000
    if (meType(flex4) > 0 || meType(block4) > 1)
      return 2400
    if (meType(block4) > 0 && meType(flex3) > 0)
      return 2000
    if (youType(flex4) > 0 || youType(block4) > 1)
      return 1200
    if (youType(block4) > 0 && youType(flex3) > 0)
      return 1000
    if (meType(flex3) > 1)
      return 400
    if (youType(flex3) > 1)
      return 200
    var score = 0
    for (i <- 0 to block4){
      score += meType(i)*meVal(i)
      score += youType(i)*youVal(i)
    }
    score
  }

  def cutCand(moves: Array[Pos], board: Board, cSize: Int): Int = {
    val me = (board.step + 1) & 1
    val you = board.step & 1
    var mSize = 0
    if (board.cand(1).score >= 2400){
      mSize += 1
      moves(1) = board.cand(1)
    } else if (board.cand(1).score == 1200) {
      mSize += 1
      moves(1) = board.cand(1)
      if (board.cand(2).score == 1200) {
        mSize += 1
        moves(2) = board.cand(2)
      }

      for (i <- mSize + 1 to cSize) {
        if (isType(board,i,block4,me) || isType(board,i,block4,you)){
          mSize += mSize
          moves(mSize) = board.cand(i)
        }
      }
    }
    mSize
  }

  def isType(board: Board, i: Int, types: Int,color: Int): Boolean = {
    board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(0) == types || board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(1) == types || board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(2) == types || board.cell(board.cand(i).row)(board.cand(i).col).pattern(color)(3) == types
  }

  def typeCount(cell: Cell, color: Int, meType: ArrayBuffer[Int]): Unit={
    cell.pattern(color).foreach(i => meType(i) = meType(i) + 1)
  }

}
