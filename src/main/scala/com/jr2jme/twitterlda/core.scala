package com.jr2jme.twitterlda

import net.java.sen.dictionary.Token
import twitter4j.conf.ConfigurationBuilder

import scala.collection.JavaConversions._
import java.io.{File, BufferedReader, StringReader}
import java.security.{MessageDigest => MD}

import net.java.sen.SenFactory
import net.java.sen.filter.stream.CompositeTokenFilter
import twitter4j._
import twitter4j.auth.AccessToken
object core {
  def main(args:Array[String]): Unit ={
    twitterstream()
  }

  def twitreco(username:String):Unit = {
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(mykey.token,mykey.tokensecret)
    twitter.setOAuthConsumer(mykey.consumer,mykey.conssecret)
    twitter.setOAuthAccessToken(accessToken)

    val listlist = twitter.getUserLists(username)
    var notexistlist=true
    var listid=0L
    for(list<-listlist){
      if(list.getName==username){
        notexistlist=false
        listid=list.getId
      }
    }
    val tagger = SenFactory.getStringTagger(null)
    val ctFillter = new CompositeTokenFilter
    //val newFile = new File(username)
    val wordcount=twitter.getUserListStatuses(listid,new Paging(1,100)).foldLeft(Map.empty[String,Int])((map,s)=>{
      //println(s.getText)

      //newFile.mkdir() //成功すればtrue, 失敗すればfalseが返る。
      ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
      tagger.addFilter(ctFillter)
      ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
      tagger.addFilter(ctFillter)

      val tokens = tagger.analyze(s.getText,new java.util.ArrayList[Token]())
      tokens.foldLeft(map)((minimap,minis)=>minimap+(minis.getSurface->(minimap.getOrElse(minis.getSurface,0)+1)))
    })
    wordcount.toSeq.sortWith(_._2 > _._2).foreach(println)
  }

  def twitlistupdate(username : String): Unit ={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(ofkey.token,ofkey.tokensecret)
    twitter.setOAuthConsumer(ofkey.consumer,ofkey.conssecret)
    twitter.setOAuthAccessToken(accessToken)
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
    var getcount=1.00d
    var sousin=1.0d
    statuses.foreach(s=>{
      if((!s.isRetweet)&&(s.getRetweetCount+s.getFavoriteCount)>2){
        sousin+=1
      }
      val retweet = s.getRetweetedStatus

      if(retweet!=null){
        //val trueretweet=twitter.showStatus(retweet.getId)
        //println(retweet.getText)
        //println(retweet.getFavoriteCount)
       // println(retweet.getRetweetCount)
        val count=retweet.getFavoriteCount+retweet.getRetweetCount
        //println(prevtext)
        //println
        getcount+=1.0
      }
      prevtext=s.getText
      /*if(s.getInReplyToStatusId!= -1){
        twitter.lookupUsers(Array(s.getInReplyToUserId)).foreach(user=> {
          if(!user.isProtected){
            val reply = twitter.showStatus(s.getInReplyToStatusId)
            val count=reply.getFavoriteCount+reply.getRetweetCount
            if(count>10){
             // println(s.getText)
              //println(reply.getText+count)
              //println
              getcount+=1.0
            }
          }
        })
      }*/
    })
    println()
    println(getcount/sousin)

    /*twitter.getFavorites(username).foreach(s=>{
      println(s.getText)
      println(s.getFavoriteCount)
      println(s.getRetweetCount)
    })*/


  }

  def twitterstream():Unit={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(ofkey.token,ofkey.tokensecret)
    twitter.setOAuthConsumer(ofkey.consumer,ofkey.conssecret)
    twitter.setOAuthAccessToken(accessToken)
    val trends = twitter.getPlaceTrends(23424856)
    val trend=trends.getTrends
    val qe = trend.foldLeft(Array.empty[String])((arr,tren)=>arr :+ tren.getName)
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

    val filter = new FilterQuery()
    filter.track(qe)
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