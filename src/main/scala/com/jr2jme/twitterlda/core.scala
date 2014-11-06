package com.jr2jme.twitterlda

import twitter4j.conf.ConfigurationBuilder

import scala.collection.JavaConversions._
import java.io.{File, BufferedReader, StringReader}
import java.security.{MessageDigest => MD}
import java.util

import net.java.sen.SenFactory
import net.java.sen.filter.stream.CompositeTokenFilter
import twitter4j._
import twitter4j.auth.AccessToken
object core {
  def main(args:Array[String]): Unit ={
    twitfavorite("NAGO_System")
  }
  def twitop(): Unit ={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(ofkey.token,ofkey.tokensecret)
    twitter.setOAuthConsumer(ofkey.consumer,ofkey.conssecret)
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

  def twitfavorite(username:String): Unit ={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(ofkey.token,ofkey.tokensecret)
    twitter.setOAuthConsumer(ofkey.consumer,ofkey.conssecret)
    twitter.setOAuthAccessToken(accessToken)
    //val username = "eiitirou"
    val statuses = twitter.getUserTimeline(username,new Paging(1,100))
    var retflag = false
    var prevtext = ""
    statuses.foreach(s=>{

      val retweet = s.getRetweetedStatus

      if(retweet!=null){
        //val trueretweet=twitter.showStatus(retweet.getId)
        println(retweet.getText)
        println(retweet.getFavoriteCount)
        println(retweet.getRetweetCount)
        val count=retweet.getFavoriteCount+retweet.getRetweetCount
        println(prevtext)
        println
      }
      prevtext=s.getText
      /*if(s.getInReplyToStatusId!= -1){
        twitter.lookupUsers(Array(s.getInReplyToUserId)).foreach(user=> {
          if(!user.isProtected){
            val reply = twitter.showStatus(s.getInReplyToStatusId)
            /*println(reply.getText)
            println(reply.getFavoriteCount)
            println(reply.getRetweetCount)*/
            val count=reply.getFavoriteCount+reply.getRetweetCount
            if(count>10){
              println(s.getText)
              println(reply.getText+count)
              println
            }
          }
        })
      }*/
    })
    println()
    /*twitter.getFavorites(username).foreach(s=>{
      println(s.getText)
      println(s.getFavoriteCount)
      println(s.getRetweetCount)
    })*/


  }

  def twitterstream():Unit={
    val builder = new ConfigurationBuilder()
    builder.setOAuthConsumerKey(mykey.consumer)
    builder.setOAuthConsumerSecret(mykey.conssecret)
    builder.setOAuthAccessToken(mykey.token)
    builder.setOAuthAccessTokenSecret(mykey.tokensecret)

    val conf = builder.build()

    // TwitterStreamのインスタンス作成
    val twitterStream = new TwitterStreamFactory(conf).getInstance()

    // Listenerを登録
    twitterStream.addListener(new Listener())
    val track = Array("http" )
    val filter = new FilterQuery()
    filter.track(track)
    filter.language(Array("ja"))
    // 実行
    twitterStream.filter(filter)
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