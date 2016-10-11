package com.bai.utils

import com.bai.model.{Board, Cell, Pos}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by linwbai on 16-10-11.
  */
object PlayUtil {

  val win = 7; // 连五

  val flex4 = 6; // 活四

  val block4 = 5; // 冲四

  val flex3 = 4; // 活三

  val block3 = 3; // 眠三

  val flex2 = 2; // 活二

  val block2 = 1; // 眠二

  val meVal = Array(0,3,7,7,18,18)

  val youVal = Array(0,2,5,5,12,12)

  def goBest(board: Board): Pos ={
    var pos = new Pos
    if (board.step == 0){
      pos.row = 7
      pos.col = 7
    }
    pos
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
