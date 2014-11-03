package com.jr2jme.twitterlda

import scala.collection.JavaConversions._
import java.io.{File, BufferedReader, StringReader}
import java.security.{MessageDigest => MD}
import java.util

import net.java.sen.SenFactory
import net.java.sen.filter.stream.CompositeTokenFilter
import twitter4j.TwitterFactory
import twitter4j.auth.AccessToken
import twitter4j.Paging
import twitter4j.Trend
object core {
  def main(args:Array[String]): Unit ={
    twitfavorite()
  }
  def twitop(): Unit ={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken("231278203-xwHCN8Mua7953jFu1r8BJcojCblojQLHkAX9D0es","t7PDmD9ijvcRQH3kDSqXre8lqHWyxNc5gxgI5j3Y")
    twitter.setOAuthConsumer("3nVuSoBZnx6U4vzUxf5w","Bcs59EFbbsdF6Sl9Ng71smgStWEGwXXKSjYvPVt7qys")
    twitter.setOAuthAccessToken(accessToken)
    val username = "JME_KH"
    //val statuses = twitter.getUserTimeline(username,new Paging(1,100))
    val listlist = twitter.getUserLists(username)
    var notexistlist=true
    var listid=0L
    for(list<-listlist){
      if(list.getName==username){
        notexistlist=false
        listid=list.getId
      }
    }
    if(notexistlist){
      listid = twitter.createUserList(username,false,"").getId
    }
    val listmembersid=twitter.getUserListMembers(listid,-1).map(a=>a.getId)
    println(listmembersid)
    val followmemberid=twitter.getFriendsIDs(username,-1).getIDs
    followmemberid.diff(listmembersid).foreach(twitter.createUserListMember(listid,_))
    //val listid = twitter.createUserList(username,false,"").getId
    //twitter.getFriendsIDs(username,-1).getIDs.foreach(twitter.createUserListMember(listid,_))
    val tagger = SenFactory.getStringTagger(null)
    val ctFillter = new CompositeTokenFilter
    val newFile = new File(username)
    newFile.mkdir() //成功すればtrue, 失敗すればfalseが返る。
    ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
    tagger.addFilter(ctFillter)
    ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
    tagger.addFilter(ctFillter)
    //twitter.getHomeTimeline.foreach(s=>println(s.getText))
    //for(s<-statuses){
      //println(s.getText)
      /*val f = new File("output.txt")
      val out = new PrintWriter("./"+username+"/"+s.getId)
      out.println(s.getText)
      out.close*/
    //}
    //val trends = twitter.getPlaceTrends(23424856)
    //val trend=trends.getTrends
    //trend.foreach(x=>println(x.getName))
    //println(trends)
    //LDA.main(Array("--dir", "./"+username+"/","--numTopics","5","0.1"))

    //val user = twitter.showUser("JME_KH")
    //println(user.g)
  }

  def twitfavorite(): Unit ={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(key.consumer,key.conssecret)
    twitter.setOAuthConsumer(key.oauth,key.oauthsecret)
    twitter.setOAuthAccessToken(accessToken)
    val username = "eiitirou"
    val statuses = twitter.getUserTimeline(username,new Paging(1,100))
    statuses.foreach(s=>{
      val flag = s.getRetweetedStatus
      if(flag!=null){
        println(flag.getText)
      }
    })
    println()
    twitter.getFavorites(username).foreach(s=>println(s.getText))

  }

  def md5hash(str:String): Unit ={
    val digester = MD.getInstance("MD5")
    digester.update(str.getBytes())
    println(digester.digest().toList.map(_&0xff).map("%02x".format(_)).mkString)
  }

}


case class Params(dir: File,
                  numTopics: Int = 20,
                  topicSmoothing: Double = .1,
                  wordSmoothing: Double = 0.1)
/*case class Version(
verson:Int,paragraphs:Set,sentences:Set,terms:Set
)*/