package com.jr2jme.twitterlda

import java.io.{PrintWriter, File}

import twitter4j.{Status, StatusAdapter}

/**
 * Created by K.H on 2014/11/04.
 */
class Listener extends StatusAdapter {
  // Tweetを受け取るたびにこのメソッドが呼び出される
  override def onStatus(status:Status):Unit= {
    if(!status.isRetweeted) {
      val newFile = new File("twitter")
      newFile.mkdir()
      val out = new PrintWriter("./twitter/twitter.corpus")
    }
  }
}