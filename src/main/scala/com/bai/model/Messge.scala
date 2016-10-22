package com.bai.model

import scala.collection.concurrent

/**
  * Created by bailinwei on 2016/10/22.
  */
sealed trait Message
case class Clean(map: concurrent.Map[String,MinMaxAI]) extends Message
