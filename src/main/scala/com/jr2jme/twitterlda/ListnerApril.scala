package com.jr2jme.twitterlda


import java.io.{PrintWriter, File}
import java.util.{Date, Calendar}
;
import org.json4s.NoTypeHints
import org.json4s.native.Serialization
import twitter4j.{GeoLocation, Status, StatusAdapter}

import scala.xml.XML

/**
 * Created by K.H on 2014/11/04.
 */

class ListenerApril(lissss:List[Long]) extends StatusAdapter {
  val newFile = new File("stream")

  newFile.mkdir()
  //val fileName = "./stream/"+status.getUser.getScreenName
  val cal = Calendar.getInstance()
  var date = (cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DATE))
  val datedir = new File("stream/"+date)
  datedir.mkdir()
  var fileName = "stream/"+date + "/"+date+".json"
  var fout = new PrintWriter(fileName)
  val lisss=lissss
  // Tweetを受け取るたびにこのメソッドが呼び出される

  override def onStatus(st:Status):Unit= {
    val nowcal = Calendar.getInstance()
    val nowdate = (nowcal.get(Calendar.YEAR)+"-"+(nowcal.get(Calendar.MONTH)+1)+"-"+nowcal.get(Calendar.DATE))
    if(date!=nowdate){
      date = nowdate
      changedate(date)
    }
    if(lisss.contains(st.getUser.getId)) {
      val stas = Statuscase(st.getId, st.getUser.getScreenName, st.getText, st.getCreatedAt, st.getGeoLocation, st.getSource)
      implicit val formats = Serialization.formats(NoTypeHints)
      val js = Serialization.write(stas)
      fout.println(js)
      println(js)
    }
  }

  def changedate(newdate:String): Unit ={
    fout.close()
    val datedir = new File("stream/"+newdate)
    datedir.mkdir()
    fileName = "stream/"+newdate + "/"+newdate+".xml"
    fout = new PrintWriter(fileName)
  }

  def fin(): Unit ={
    fout.close
  }
}