package com.jr2jme.twitterlda

import java.util.Random
import scala.io.Source
import scala.math
import chalk.topics.LDA
import dispatch._
import net.java.sen.dictionary.Token
import org.knowceans.corpus.{VisCorpus, NumCorpus}
import org.knowceans.util.{StopWatch, CokusRandom}
import twitter4j.conf.ConfigurationBuilder

import scala.collection.JavaConversions._
import java.io._
import java.security.{MessageDigest => MD}

import net.java.sen.SenFactory
import net.java.sen.filter.stream.CompositeTokenFilter
import twitter4j._
import twitter4j.auth.AccessToken
import org.knowceans.topics.simple.{TopicMatrixPanel, IldaGibbs}

import scala.xml.XML

object core {
  val twitter = TwitterFactory.getSingleton
  val tagger = SenFactory.getStringTagger(null)
  val ctFillter = new CompositeTokenFilter
  var objtwts:ResponseList[Status] = null

  def open(fileName:String)(body:BufferedReader => Unit) : Unit = {
    // ディスクへの細かなアクセスを避けるため、バッファを介してファイルを読む
    val in = new BufferedReader(new FileReader(fileName))
    try
      body(in)
    finally
      in.close  // 開けたら閉じる
  }


  def main(args:Array[String]): Unit ={
    twitter.setOAuthConsumer(botkey.consumer,botkey.conssecret)
    val accessToken = new AccessToken(botkey.token,botkey.tokensecret)
    twitter.setOAuthAccessToken(accessToken)
    ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
    tagger.addFilter(ctFillter)
    ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
    tagger.addFilter(ctFillter)
    //hdplda(twitlda("JME_KH"))
    //hdplda(twitlda("renho_sha"))
    /*var page = new Paging(1,200,1L)
    for(i<-(1 to 10)) {
      twitter.getUserTimeline("elephas_Koji", page).foreach(s=>{
        println(s.getCreatedAt + s.getText)
        page.setMaxId(s.getId)
        println(s.getId)
      })
    }*/
    print("input=")
    val lines = Iterator.continually(readLine()).takeWhile((s=>{
      s!=null&&s!=""
    }) )
    lines.foreach(s=>{
      if(s!="") {
        //getusertweet(s)
        //twitidf(s)
        val tweets = getusertweet(s,false).toList
        val twcount = twitcount(tweets,Map.empty[String,Int])
        val topictweetcount = tweets.filter(_.getText.contains("消費税")).filter(!_.isRetweet)
        /*val df = twcount._1.values.foldLeft(Map.empty[String,Int])((map,cw)=>{
          cw.foldLeft(map)((minimap,ho)=>{
            minimap+(ho._1->(ho._2+minimap.getOrElse(ho._1,0)))
          })
        })*/
        //val topictweets = topictweetcount.keySet
        val seqnp=topictweetcount.foldLeft(Seq.empty[Double])((se,st)=>{
          val mixdic = readdic_kobayashi().foldLeft(readdic_takamura())((taka,koba)=>{
            taka+koba
          })
          se :+ negaposi(st,mixdic,twcount._2)
        })
        println(topictweetcount(changepoint(seqnp,3)).getText)
      }
    })
    //twitterstream()
  }

  def changepoint(se:Seq[Double],depth:Int):Int= {
    //if (depth != 0) {
      val avg = se.sum / se.length.toDouble
      val sx = se.foldLeft(Seq.empty[Double])((ss,va)=>ss:+(ss.sum+va-avg))
      val sxabs = se.foldLeft(Seq.empty[Double])((ss,va)=>ss:+va.abs)
      val sdiff = sx.max-sx.min
      var index = -1
    println(sxabs.indexWhere(_==sxabs.max)+"/"+sx.length)
      sxabs.indexWhere(_==sxabs.max)

    //}
   // else {
   //     0
   // }
  }

  def makefilelist(dir:String): Unit ={
    val out = new PrintWriter("./2014-12-23")

    new File(dir).listFiles.foreach(file=>{
      //val xml=XML.loadFile(file.getPath)
      //println(xml)
      out.println(file.getPath)
    })
    out.close()
  }

  def twitidf(date:String): Unit ={
    open(date) { f =>
      def loop():Unit ={

        var line = f.readLine  // 一行ずつ読む
        while(line != null){  // nullが返ると読み込み終了
        // use read data here
          println(XML.loadFile(line).text)
          line = f.readLine
        }
      }
      loop
    }
  }
  def twitcount(lstat:List[Status],idf:Map[String,Int]): (Map[Status,Map[String,Int]],Map[String,Int]) ={//idf数えるようと個々のツイートに対する単語出現回数
    lstat.foldLeft((Map.empty[Status,Map[String,Int]],idf))((ddd,st)=> {
      var wordset = Set.empty[String]
      val tokens = tagger.analyze(st.getText, new java.util.ArrayList[Token]())
      val twitmap = tokens.foldLeft(Map.empty[String,Int],ddd._2)((konomap, tok) => {
        val word = tok.getSurface
        if(!wordset.contains(word)){
          wordset = wordset+word
          (konomap._1 + (word -> (konomap._1.getOrElse(word, 0) + 1)),konomap._2+(word->(konomap._2.getOrElse(word,0)+1)))
        }else {
          (konomap._1 + (word -> (konomap._1.getOrElse(word, 0) + 1)),konomap._2)
        }
      })
      (ddd._1+(st->twitmap._1),twitmap._2)
    })
  }

  def twitsearch(word:String): List[Status] ={
    println("searching")
    val query = new Query
    query.setQuery(word)
    var ser = twitter.search(query)
    println(ser)
    ser.getTweets.foreach(s=>println(s))
    while(ser.nextQuery()!=null){
      ser = twitter.search(ser.nextQuery())
      ser.getTweets.foreach(s=>println(s.getText))
      println(ser.getRefreshURL)
    }
    ser.getTweets.toList
  }


  /*def getnegaposi(tweet:Status,dicmap:Map[String,Double],phi:Array[Array[Double]],topic:Int,wordmap:Map[String,Int]) : Double = {
    val text = tweet.getText
    val tagger = SenFactory.getStringTagger(null)
    val ctFillter = new CompositeTokenFilter
    ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
    tagger.addFilter(ctFillter)
    ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
    tagger.addFilter(ctFillter)
    val tokens = tagger.analyze(text,new java.util.ArrayList[Token]())
    tokens.foldLeft(0d)((va,tok)=>{
      if(topic != -1&&wordmap.contains(tok.getSurface)) {
        va + (dicmap.getOrElse(tok.getSurface, 0d)/(-Math.log(phi(topic)(wordmap.getOrElse(tok.getSurface, 1)))))
      }
      else{
        va + (dicmap.getOrElse(tok.getSurface, 0d))
      }//0でないやつの数を数えたい
    })
  }*/

  def negaposi(tweet:Status,dicmap:Map[String,Double],wordmap:Map[String,Int]) : Double = {
    val text = tweet.getText
    val tagger = SenFactory.getStringTagger(null)
    val ctFillter = new CompositeTokenFilter
    ctFillter.readRules(new BufferedReader(new StringReader("名詞-数")))
    tagger.addFilter(ctFillter)
    ctFillter.readRules(new BufferedReader(new StringReader("記号-アルファベット")))
    tagger.addFilter(ctFillter)
    val tokens = tagger.analyze(text,new java.util.ArrayList[Token]())
    tokens.foldLeft(0d)((va,tok)=>{
      if(wordmap.contains(tok.getSurface)) {
        //println("logidf="+Math.log(wordmap.getOrElse(tok.getSurface, 1).toDouble))
        va + (dicmap.getOrElse(tok.getSurface, 0d) /Math.log((wordmap.getOrElse(tok.getSurface, 1).toDouble+1d)))

      }
      else{
        va + (dicmap.getOrElse(tok.getSurface, 0d))
      }//0でないやつの数を数えたい
    })
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
          val atai=if(linearray(0).startsWith("ネガ")){//重みはトピックごとに変えないとダメ
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


  def getusertweet(username:String,outfile:Boolean): Seq[Status] ={

    val tweets = (1 to 16).foldLeft(Seq.empty[Status])((list,pnumber)=> {
      list++twitter.getUserTimeline(username, new Paging(pnumber, 200)).toSeq
    })

    val newFile = new File(username)
    newFile.mkdir()
    if(outfile) {
      tweets.foreach(tw => {
        val out = new PrintWriter("./" + username + "/" + tw.getId.toString)
        out.println(tw)
        out.close()
      })
    }
    println("fin")
    tweets
  }

  def hdplda(maaa:Map[String,Int]): Unit ={
    val niter: Int = 200
    val niterq: Int = 10
    val filebase: String = "twitter/twitter"
    // file or synthetic
    val usefile: Boolean = true
    // topic display panel
    val display: Boolean = true
    val rand: Random = new CokusRandom(56567651)
    var corpus: NumCorpus = null

    corpus = new NumCorpus(filebase + ".corpus")

    // corpus.reduce(100, rand);
    corpus.split(10, 2, rand)
    val train: NumCorpus = corpus.getTrainCorpus.asInstanceOf[NumCorpus]
    val test: NumCorpus = corpus.getTestCorpus.asInstanceOf[NumCorpus]

    val w: Array[Array[Int]] = train.getDocWords(rand)
    val wq: Array[Array[Int]] = test.getDocWords(rand)
    val K0: Int = 0

    val V: Int = corpus.getNumTerms
    val alpha: Double = 1.
    // beta = 1 --> K = 12,.5-->16, .1-->26@200, 75@500, 115@645 (beta
    // should be larger),
    //
    val beta: Double = .1
    val gamma: Double = 1.5
    // run sampler
    val gs = new IldaGibbs(w, wq, K0, V, alpha, beta, gamma, rand)
    gs.init
    System.out.println("initialised")
    System.out.println(gs)
    // initial test
    //gs.initq
    //gs.runq(niterq)
    //System.out.println("perplexity = " + gs.ppx)
    StopWatch.start
    System.out.println("starting Gibbs sampler with " + niter + " iterations")

    gs.run(niter)
    System.out.println(StopWatch.format(StopWatch.stop))
    //gs.initq

    //System.out.println("perplexity = " + gs.ppx)
    //System.out.println(gs)
    gs.packTopics
    System.out.println("finished")
    System.out.println(gs)





    try {
      val bw: PrintStream = new PrintStream(filebase + ".ilda.result")
      gs.print(bw, filebase, corpus.getOrigDocIds()(0), maaa.size)
      bw.close
      System.out.println("done")
    }
    catch {
      case e: FileNotFoundException => {
        e.printStackTrace
      }
    }
    val phi = gs.getphi()//トピックごとの単語が選ばれる確率
    val theta = gs.gettheta();//文書ごとのトピックが選ばれる確率(見るのはとりあえず一番の人)
    var c = 0
    phi.foreach(topic=>{
      println("topic="+c)
      val so = maaa.toSeq.sortWith((a,b)=>topic(a._2)>topic(b._2))
      for(i <- (0 to 10)){
        println(so(i)._1+topic(so(i)._2))
      }
      println
      c+=1
    })
    val se = objtwts.foldLeft(new Array[Map[Status,Int]](phi.length))((tpse,twt)=>{
        val tok = tagger.analyze(twt.getText,new java.util.ArrayList[Token]())
        var maxi = -1
        var maxp = Double.MinValue
        for(i<-(0 to theta(0).length-1)){
          val prob = tok.foldLeft(Math.log(theta(0)(i)))((va,wa)=>{
            val key = maaa.getOrElse(wa.getSurface,-1)
            if(key!= -1){
              va+Math.log(phi(i)(key))//誤差
            }
            else{
              va
            }
          })
          //println(prob)
          if(maxp != null) {
            if (maxp < prob) {
              maxp = prob
              maxi = i
            }
          }
          else{
            maxp=prob
            maxi = i
          }
        }
        //println("max topic = "+maxi)//todo 変化
        if(maxi!= -1) {
          if (tpse(maxi) == null) {
            tpse(maxi) = Map.empty[Status,Int]
          }
          tpse(maxi) = tpse(maxi) + (twt->maxi)
        }
        tpse
      })
    var count =0
    for(i<-se){
      println("topic"+count)
      if(i!=null) {
        for (j <- i) {
          println(j._1.getCreatedAt+j._1.getText)
          val mixdic = readdic_kobayashi().foldLeft(readdic_takamura())((taka,koba)=>{
            taka+koba
          })
          //println(getnegaposi(j._1,mixdic,phi,j._2,maaa))//map[Map[Status,Double]]
        }
      }
      println
      println
      count+=1
    }
  }

  /*def wordweight(wordmap:Map[String,Int],phi:Array[Array[Double]]): Map[String,Array[Double]] ={
    wordmap.foldLeft(Map.empty[String,Double])((ma,i)=>{
      val hoge = phi.foldLeft(0d)((prob,j)=> {
        prob-Math.log10(j(i._2))
      })
      ma+(i._1->hoge)
    })
  }*/

  def twitlda(username:String): Map[String,Int]= {

    val wordmap=scala.collection.mutable.Map.empty[String,Int]

    val newFile = new File("twitter")
    newFile.mkdir()
    val out = new PrintWriter("./twitter/twitter.corpus")

    val utweet = (1 to 1).foldLeft(Map.empty[Int, Int])((mmm, s) => {
      val block = makebog(username,wordmap,s,true)
      block.foldLeft(mmm)((mal, xx) => {
        mal + (xx._1 -> (mal.getOrElse(xx._1, 0) + xx._2))
      })

    })
    out.print(utweet.size)
    utweet.toSeq.sortWith(_._2 > _._2).foreach(s=>out.print(" " + s._1+":"+s._2))
    out.println

    open("username.txt") { f =>
      def loop():Unit ={
        var line = f.readLine  // 一行ずつ読む
        while(line != null) {
          // nullが返ると読み込み終了
          // use read data here
          val usertweet = (1 to 1).foldLeft(Map.empty[Int, Int])((mmm, s) => {
            val block = makebog(line,wordmap,s,false)
            block.foldLeft(mmm)((mal, xx) => {
              mal + (xx._1 -> (mal.getOrElse(xx._1, 0) + xx._2))
            })
          })
          out.print(usertweet.size)
          usertweet.toSeq.sortWith(_._2 > _._2).foreach(s=>out.print(" " + s._1+":"+s._2))
          out.println
          line=f.readLine()
        }
      }
      loop
    }


    /*val frlist= twitter.getFriendsList(username,-1)
    frlist.foreach(s=> {
      val objname = s.getScreenName
      val usertweet = (1 to 1).foldLeft(Map.empty[Int, Int])((mmm, s) => {
        val block = makebog(objname,wordmap,s,false)
        block.foldLeft(mmm)((mal, xx) => {
          mal + (xx._1 -> (mal.getOrElse(xx._1, 0) + xx._2))
        })
      })
      out.print(usertweet.size)
      usertweet.toSeq.sortWith(_._2 > _._2).foreach(s=>out.print(" " + s._1+":"+s._2))
      out.println
    })*/

    /*val frlist2=twitter.getFriendsList(username,frlist.getNextCursor)
    frlist2.foreach(s=> {
      val objname = s.getScreenName
      val usertweet = (1 to 1).foldLeft(Map.empty[Int, Int])((mmm, s) => {
        val block = makebog(objname,wordmap,s)
        block.foldLeft(mmm)((mal, xx) => {
          mal + (xx._1 -> (mal.getOrElse(xx._1, 0) + xx._2))
        })
      })
      out.print(usertweet.size)
      usertweet.toSeq.sortWith(_._2 > _._2).foreach(s=>out.print(" " + s._1+":"+s._2))
      out.println
    })*/
    out.close
    wordmap.toMap
  }

  def makebog(username:String,map:scala.collection.mutable.Map[String,Int],s:Int,obj:Boolean): Map[Int,Int] ={
    val twts = twitter.getUserTimeline(username, new Paging(s, 100))
    if(obj){
      objtwts = twts
    }
    twts.foldLeft(Map.empty[Int, Int])((ma, x) => {
      val tweet = x.getText.replaceAll("(\\w+?)://[\\w/:%#\\$&\\?\\(\\)~\\.=\\+\\-]+","").replaceAll("@[\\w/:%#\\$&\\?\\(\\)~\\.=\\+\\-]+","")
      val tokens = tagger.analyze(tweet, new java.util.ArrayList[Token]())
      val docmap = tokens.foldLeft(Map.empty[Int, Int])((konomap, tok) => {
        if(tok.getMorpheme.getPartOfSpeech.contains("名詞")) {
          val word = tok.getSurface
          if (!map.contains(word)) {
            map.put(word, map.size)
          }
          val key = map.getOrElse(word, map.size)
          konomap + (key -> (konomap.getOrElse(key, 0) + 1))
        }
        else{
          konomap
        }
      })
      docmap.foldLeft(ma)((mal, xx) => {
        mal + (xx._1 -> (mal.getOrElse(xx._1, 0) + xx._2))
      })
    })
  }

  def twitreco(username:String):Unit = {

    val listlist = twitter.getUserLists(username)
    var notexistlist=true
    var listid=0L
    for(list<-listlist) {
      if (list.getName == username) {
        notexistlist = false
        listid = list.getId
      }
    }

    //val newFile = new File(username)
    val wordcount=twitter.getUserListStatuses(listid,new Paging(1,100)).foldLeft(Map.empty[String,Int])((map,s)=>{
      //println(s.getText)
      //newFile.mkdir() //成功すればtrue, 失敗すればfalseが返る。
      val tokens = tagger.analyze(s.getText,new java.util.ArrayList[Token]())
      tokens.foldLeft(map)((minimap,minis)=>minimap+(minis.getSurface->(minimap.getOrElse(minis.getSurface,0)+1)))
    })
    wordcount.toSeq.sortWith(_._2 > _._2).foreach(println)
  }

  /*def twittertopic(username : String): Unit ={
    val newFile = new File(username)
      //println(s.getText)
    newFile.mkdir() //成功すればtrue, 失敗すればfalseが返る。
    for(num<-(1 to 10)) {
      val statuses=twitter.getUserTimeline(username, new Paging(num, 200))
      for(s<-statuses) {
        //println(s.getText)
        //val f = new File("output.txt")
        val out = new PrintWriter("./" + username + "/" + s.getId)
        out.println(s.getText)
        out.close
      }
    }
      val trends = twitter.getPlaceTrends(23424856)
      val trend=trends.getTrends
      trend.foreach(x=>println(x.getName))
      println(trends)
      LDA.main(Array("--dir", "./"+username+"/","--numTopics","2","0.1"))
  }*/

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
    //val trends = twitter.getPlaceTrends(23424856)
    //val trend=trends.getTrends
    //val qe = trend.foldLeft(Array.empty[String])((arr,tren)=>arr :+ tren.getName)
    //val qe = Array("")

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
    //filter.track(qe)
    //filter.language(Array("ja"))
    // 実行
    //twitterStream.filter(filter)
    twitterStream.sample()
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