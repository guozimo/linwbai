package com.bai.model


/**
  * 棋子
  * Created by linwbai on 16-10-11.
  */
class Pos {
  var row: Int = _
  var col: Int = _
  var value: Int = _ // 0 无 1 黑(AI) 2 白
  var score: Int = _

  def equalsVal(obj: Pos): Boolean = {
    obj.row == row && obj.col == col
  }
}
