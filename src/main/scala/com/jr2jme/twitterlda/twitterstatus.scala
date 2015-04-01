package com.jr2jme.twitterlda

import java.io.PrintWriter
import java.util.Date

import twitter4j.{GeoLocation, Status}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods
import org.json4s.native.Serialization

/**
 * Created by Hirotaka on 2015/04/01.
 */
case class Twitterstatus (status:List[Statuscase])
case class Statuscase(id:Long,name:String,text:String,date:Date,loc: GeoLocation,apli:String)
case class Userstatus (name:String,status:List[Statuscase])

class twitetrstatus{
  def tofile(staas:List[Status]): Unit ={
    val slist = staas.foldLeft(List.empty[Statuscase])((lis,st)=>{
      Statuscase(st.getId,st.getUser.getScreenName,st.getText,st.getCreatedAt,st.getGeoLocation,st.getSource)::lis
    })

    val stas = Twitterstatus(slist)
    implicit val formats = Serialization.formats(NoTypeHints)
    val js = Serialization.write(stas)
    val out = new PrintWriter("hoge.txt")
    out.println(js)
    out.close


  }
  def tofilepeuser(staas:List[Status],uname:String): Unit ={
    val slist = staas.foldLeft(List.empty[Statuscase])((lis,st)=>{
      Statuscase(st.getId,st.getUser.getScreenName,st.getText,st.getCreatedAt,st.getGeoLocation,st.getSource)::lis
    })

    val stas = Userstatus(uname,slist)
    implicit val formats = Serialization.formats(NoTypeHints)
    val js = Serialization.write(stas)
    val out = new PrintWriter(uname+"_twitter.json")
    out.println(js)
    out.close


  }
  // ここに、テストコードを書く！
}
