package com.jr2jme.twitterlda


import java.io.{PrintWriter, File}
import java.util.Calendar
;
import twitter4j.{Status, StatusAdapter}

import scala.xml.XML

/**
 * Created by K.H on 2014/11/04.
 */
class Listener extends StatusAdapter {
  val newFile = new File("stream")

  newFile.mkdir()
  //val fileName = "./stream/"+status.getUser.getScreenName
  val cal = Calendar.getInstance()
  var date = (cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DATE))
  val datedir = new File("stream/"+date)
  datedir.mkdir()
  var fileName = "stream/"+date + "/"+date+".xml"
  var fout = new PrintWriter(fileName)
  val sentou = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<field>\n"
  fout.println(sentou)
  // Tweetを受け取るたびにこのメソッドが呼び出される
  override def onStatus(status:Status):Unit= {
    val nowcal = Calendar.getInstance()
    val nowdate = (nowcal.get(Calendar.YEAR)+"-"+(nowcal.get(Calendar.MONTH)+1)+"-"+nowcal.get(Calendar.DATE))
    if(date!=nowdate){
      date = nowdate
      changedate(date)
    }
    if(status.getLang=="ja") {
      // 書き込み処理
      val xmlst = "<tweetstatus user = \""+status.getUser.getScreenName+"\" date = \""+status.getCreatedAt+"\" id = \""+status.getId+"\" replyusername = \""+ status.getInReplyToScreenName+"\" retweetusername = \""+ (if(status.isRetweeted)status.getRetweetedStatus.getUser.getScreenName else "")+"\">" +
        status.getText+
        "</tweetstatus>"//リツイート元と返信先もとっておきたい
      fout.println(xmlst)
      //val xml = XML.loadString(xmlst)
      //println(xml.toString)
      //XML.save(fileName,xml,"UTF-8",true,null)
    }
  }

  def changedate(newdate:String): Unit ={
    fout.println("\n</field>")
    fout.close()
    val datedir = new File("stream/"+newdate)
    datedir.mkdir()
    fileName = "stream/"+newdate + "/"+newdate+".xml"
    fout = new PrintWriter(fileName)
    fout.println(sentou)
  }

  def fin(): Unit ={
    fout.println("\n</field>")
    fout.close
  }
}