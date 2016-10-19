package com.bai.model

/**
  * Created by bailinwei on 16/10/15.
  */
class MinMaxAI {

  val wins = Array.ofDim[Boolean](15,15,572)

  var count = 0

  val myWin = Array.fill(572)(0)

  val airingWin = Array.fill(572)(0)

  val chessBoard = Array.fill(15)(Array.fill(15)(0))

  var over = false

  val start_time = System.currentTimeMillis()

  initWin()

  def initWin(): Unit = {
    for (i <- 0 until 15;j <- 0 until 11) {
      for (k <- 0 until 5){
        wins(i)(j + k)(count) = true
      }
      count += 1
    }

    for (i <- 0 until 15;j <- 0 until 11) {
      for (k <- 0 until 5){
        wins(j + k)(i)(count) = true
      }
      count += 1
    }

    for (i <- 0 until 11;j <- 0 until 11) {
      for (k <- 0 until 5){
        wins(i + k)(j + k)(count) = true
      }
      count += 1
    }

    for (i <- 0 until 11;j <- Range(14,3,-1)) {
      for (k <- 0 until 5){
        wins(i + k)(j - k)(count) = true
      }
      count += 1
    }
  }

  def play(pos: Pos): Unit ={
    chessBoard(pos.row)(pos.col) = pos.value
    for (k <- 0 until count) {
      if (wins(pos.row)(pos.col)(k)) {
        pos.value match {
          case 1 => {
            myWin(k) += 1
            airingWin(k) = 6
          }
          case 2 => {
            airingWin(k) += 1
            myWin(k) = 6
          }
        }
        if (airingWin(k) == 5 || myWin(k) == 5) {
          over = true
        }
      }
    }
  }

  def getPlay: Pos = {
    val pos = new Pos
    val myScore = Array.fill(15)(Array.fill(15)(0))
    val airingScore = Array.fill(15)(Array.fill(15)(0))
    var max = 0
    for (i <- 0 until 15;j <- 0 until 15) {
      if (chessBoard(i)(j) == 0){
        for (k <- 0 until count){
          if (wins(i)(j)(k)) {
            myWin(k) match {
              case 1 => myScore(i)(j) += 200
              case 2 => myScore(i)(j) += 400
              case 3 => myScore(i)(j) += 2000
              case 4 => myScore(i)(j) += 10000
              case _ =>
            }
            airingWin(k) match {
              case 1 => airingScore(i)(j) += 220
              case 2 => airingScore(i)(j) += 420
              case 3 => airingScore(i)(j) += 2100
              case 4 => airingScore(i)(j) += 20000
              case _ =>
            }
          }
        }
        if (myScore(i)(j) > max) {
          max = myScore(i)(j)
          pos.row = i
          pos.col = j
        } else if (myScore(i)(j) == max) {
          if (airingScore(i)(j) > airingScore(pos.row)(pos.col)) {
            pos.row = i
            pos.col = j
          }
        }

        if (airingScore(i)(j) > max) {
          max  = airingScore(i)(j)
          pos.row = i
          pos.col = j
        } else if (airingScore(i)(j) == max) {
          if (myScore(i)(j) > myScore(pos.row)(pos.col)) {
            pos.row = i
            pos.col = j
          }
        }
      }
    }
    pos
  }

}
