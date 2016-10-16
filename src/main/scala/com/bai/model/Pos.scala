package com.bai.model

import scala.beans.BeanProperty


/**
  * 棋子
  * Created by linwbai on 16-10-11.
  */
class Pos {
  @BeanProperty
  var row: Int = _
  @BeanProperty
  var col: Int = _
  @BeanProperty
  var value: Int = 2 // 0 无 1 白(AI) 2 黑
  @BeanProperty
  var score: Int = _

  def equalsVal(obj: Pos): Boolean = {
    obj.row == row && obj.col == col
  }
}
