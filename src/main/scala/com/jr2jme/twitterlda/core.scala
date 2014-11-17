package com.jr2jme.twitterlda

import dispatch._
import net.java.sen.dictionary.Token
import twitter4j.conf.ConfigurationBuilder

import scala.collection.JavaConversions._
import java.io.{FileReader, File, BufferedReader, StringReader}
import java.security.{MessageDigest => MD}

import net.java.sen.SenFactory
import net.java.sen.filter.stream.CompositeTokenFilter
import twitter4j._
import twitter4j.auth.AccessToken


object core {
  def open(fileName:String)(body:BufferedReader => Unit) : Unit = {
    // ディスクへの細かなアクセスを避けるため、バッファを介してファイルを読む
    val in = new BufferedReader(new FileReader(fileName))
    try
      body(in)
    finally
      in.close  // 開けたら閉じる
  }


  def main(args:Array[String]): Unit ={
    val tweets = getusertweet("renho_sha")
    println(tweets.length)
    val dicmap = readdic_kobayashi()

    tweets.foreach(s=>{

      if(s.getText.contains("補正予算")) {
        println(getnegaposi(s, dicmap) + s.getText)
      }
    })
  }
  def getnegaposi(tweet:Status,dicmap:Map[String,Double]) : Double = {
    val text = tweet.getText
    val tagger = SenFactory.getStringTagger(null)
    val ctFillter = new CompositeTokenFilter
    ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
    tagger.addFilter(ctFillter)
    ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
    tagger.addFilter(ctFillter)
    val tokens = tagger.analyze(text,new java.util.ArrayList[Token]())
    tokens.foldLeft(0d)((va,tok)=>{
      va+dicmap.getOrElse(tok.getSurface,0d)//0でないやつの数を数えたい
    })/tokens.size()
  }
  def readdic_takamura(): Map[String,Double] ={//http://www.lr.pi.titech.ac.jp/~takamura/pndic_ja.html
    //for(line <- Source.fromFile("").getLines) {println(line)}
    var map=Map.empty[String,Double]
    open("pn_ja-utf.dic") { f =>
      def loop():Map[String,Double] ={

        var line = f.readLine  // 一行ずつ読む
        while(line != null){  // nullが返ると読み込み終了
          // use read data here
          val linearray=line.split(":")
          //println(linearray(0)+" : "+linearray(3))
          //println(map.size)
          map=map+(linearray(0)->linearray(3).toDouble)
          line=f.readLine()
        }
        map
      }
      loop
    }
  map
  }
  def readdic_kobayashi(): Map[String,Double] ={//http://www.lr.pi.titech.ac.jp/~takamura/pndic_ja.html
  //for(line <- Source.fromFile("").getLines) {println(line)}
  var map=Map.empty[String,Double]
    open("wago.121808.pn") { f =>
      def loop():Map[String,Double] ={

        var line = f.readLine  // 一行ずつ読む
        while(line != null){  // nullが返ると読み込み終了
        // use read data here
        val linearray=line.split("\t")
          //println(linearray(0)+" : "+linearray(3))
          //println(map.size)
          //println(linearray(1))
          val atai=if(linearray(0).startsWith("ネガ")){
            -1
          }else{
            1
          }
          map=map+(linearray(1).split(" ")(0)->atai)
          line=f.readLine()
        }
        map
      }
      loop
    }
    map
  }


  def getusertweet(username:String): ResponseList[Status] ={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(mykey.token,mykey.tokensecret)
    twitter.setOAuthConsumer(mykey.consumer,mykey.conssecret)
    twitter.setOAuthAccessToken(accessToken)
    //(1 to 10).fold(Seq.empty[ResponseList[Status]])((pnumber,list)=> list:+twitter.getUserTimeline(username, new Paging(1, 100)))
    var tweets:ResponseList[Status]=null
    for(s<-(1 to 10)) {
      if(tweets==null){
        tweets=twitter.getUserTimeline(username, new Paging(s, 100))
      }
      else{
        twitter.getUserTimeline(username, new Paging(s, 100)).foreach(x=>tweets.add(x))
      }
    }
    tweets
  }
  def gettogepage():Unit = {
   /* val urlori="http://togetter.com/api/moreTweets/"+742165
    val http = new Http
    val request = url(urlori)
    val requestWithParams =
      request
        .POST
        .addParameter("page", "1")
        .addParameter("key", "")
    val req = :/("api.tumblr.com") / "v2" / "blog" / "pab-tech.tumblr.com" / "avatar"
    //val handler = req >>> new java.io.FileOutputStream("cat.png")
    http(requestWithParams)
*/

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
    ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
    tagger.addFilter(ctFillter)
    ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
    tagger.addFilter(ctFillter)

    //val newFile = new File(username)
    val wordcount=twitter.getUserListStatuses(listid,new Paging(1,100)).foldLeft(Map.empty[String,Int])((map,s)=>{
      //println(s.getText)
      //newFile.mkdir() //成功すればtrue, 失敗すればfalseが返る。
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

    twitter.getFavorites(username).foreach(s=>{
      println(s.getText)
      println(s.getFavoriteCount)
      println(s.getRetweetCount)
    })


  }

  def twitterstream():Unit={
    val twitter = TwitterFactory.getSingleton
    val accessToken = new AccessToken(ofkey.token,ofkey.tokensecret)
    twitter.setOAuthConsumer(ofkey.consumer,ofkey.conssecret)
    twitter.setOAuthAccessToken(accessToken)
    //val trends = twitter.getPlaceTrends(23424856)
    //val trend=trends.getTrends
    //val qe = trend.foldLeft(Array.empty[String])((arr,tren)=>arr :+ tren.getName)
    val qe = Array("amazon")

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