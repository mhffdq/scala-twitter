package com.jr2jme.twitterlda


import java.io.File
import java.util.Calendar
;
import twitter4j.{Status, StatusAdapter}

import scala.xml.XML

/**
 * Created by K.H on 2014/11/04.
 */
class Listener extends StatusAdapter {
  // Tweetを受け取るたびにこのメソッドが呼び出される
  override def onStatus(status:Status):Unit= {
    if(!status.isRetweeted&& !status.getText.contains("http") && status.getLang=="ja") {
      val newFile = new File("stream")

      newFile.mkdir()
      //val fileName = "./stream/"+status.getUser.getScreenName
      val cal = Calendar.getInstance()
      val date = cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DATE)
      val datedir = new File("stream/"+date)
      datedir.mkdir()
      val fileName = "stream/"+date+"/"+status.getId
      val encode = "UTF-8"
      val append = true

      // 書き込み処理
      val xmlst = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
        "<tweetstatus user = \""+status.getUser.getScreenName+"\" date = \""+status.getCreatedAt+"\">" +
        status.getText+
        "</tweetstatus>"
      val xml = XML.loadString(xmlst)
      //println(xml.toString)
      XML.save(fileName,xml,"UTF-8",true,null)
    }
  }
}