package com.bai.web

import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.{RequestMapping, RequestMethod}

/**
  * Created by bailinwei on 16/10/7.
  */
@Controller
@RequestMapping
class MainPageController {

  @RequestMapping(method = Array(RequestMethod.GET))
  def home() : String = {
    "home.html"
  }
}
