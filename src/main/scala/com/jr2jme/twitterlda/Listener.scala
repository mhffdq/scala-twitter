package com.jr2jme.twitterlda

import twitter4j.{Status, StatusAdapter}

/**
 * Created by K.H on 2014/11/04.
 */
class Listener extends StatusAdapter {
  // Tweetを受け取るたびにこのメソッドが呼び出される
  override def onStatus(status:Status):Unit= {
    if(status.getLang=="ja"&& status.isRetweet) {
      println(status.getUser)
      println(status.getRetweetedStatus.getUser)
      println()
    }
  }
}