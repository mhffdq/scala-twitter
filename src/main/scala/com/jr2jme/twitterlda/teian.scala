package com.jr2jme.twitterlda

import java.io.StringReader
import java.net.URL

import net.java.sen.dictionary.Token
import org.xml.sax.InputSource
import twitter4j.Status
import scala.collection.JavaConversions._
import scala.io.Source
import scala.xml.{Node, XML}
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml.{ NodeSeq, Elem }
import dispatch._, Defaults._
/**
 * Created by Hirotaka on 2014/12/30.
 */
object teian {
  val dicmap = core.readdic_kobayashi_mei()
  val dicyou = core.readdic_kobayashi_you()
  var idflist:List[Status]=null

  def getnegaposi_kyori(text:String,sea:String) : Double = {
    val p = text.indexOfSlice(sea)
    val tokens = core.tagger.analyze(text,new java.util.ArrayList[Token]())
    var mae = 0
    var va = 0d
    var count =0
    for(tok<-tokens){
      if(tok.getStart<p){
        mae +=1
      }
    }
    for(i <- (0 to tokens.length-1)) {
      val tok = tokens.get(i)

      val word = if (tok.getMorpheme.getBasicForm == "*") tok.getSurface else tok.getMorpheme.getBasicForm
      var check = true


      if (dicyou.contains(word)) {
        val se = dicyou.get(word).get
        var j = 1
        var fil = se
        var loop = true
        if(se.size == 1){
          if(fil.toSeq(0).length==2) {
            va += fil.toSeq(0).last.toInt.toDouble
            count += 1
          }
          check = false
        }
        while (check && loop) {
          if (i + j <= tokens.length - 1) {
            val tt = tokens.get(i + j)
            val wor = if (tt.getMorpheme.getBasicForm == "*") tt.getSurface else tt.getMorpheme.getBasicForm
            fil = fil.filter(s => s(j) == wor || s(j) == tt.getSurface)
            if (fil.size == 1) {
              if (fil.toSeq(0).length == j + 2) {
                va += fil.toSeq(0).last.toInt.toDouble
                count += 1
                check = false
              }
            } else if (fil.size == 0) {
              loop = false
            }
            j += 1
          } else {
            loop = false
          }
        }
        if (check) {
          if (dicmap.contains(word)) {
            count += 1
            val atai = dicmap.getOrElse(word, 0d)
            va += (atai)
            //print(i-mae)
          }

        }
      } else{
        if (dicmap.contains(word)) {
          count += 1
          val atai = dicmap.getOrElse(word,0d)
          va += (atai)
          //print((i-mae))
        }

      }
    }
    println(va+":"+count)
    if(count!=0) {
      va / count
    }else{
      0d
    }
  }

  def getnegaposi_gyou(text:String,sea:String,meisiomomi:Map[String,Double]) : Double = {//極性をもった単語が同じ行とかそういうのを見る
    val p = text.indexOfSlice(sea)
    val sptexts=text.split("\n")
    var count =0
    var zentairyou=0d
    val nnnppp = sptexts.foldLeft(0d)((vvv,s)=>{//行ごとに処理
      val tokens = core.tagger.analyze(s,new java.util.ArrayList[Token]())
      var mae = 0
      var va = vvv
      for(tok<-tokens){
        if(tok.getStart<p){
          mae +=1
        }
      }
      var maxmeisival = 0d
      var meisiflag = false
      for(i <- (0 to tokens.length-1)) {//単語ごとに処理
        val tok = tokens.get(i)
        val word = if (tok.getMorpheme.getBasicForm == "*") tok.getSurface else tok.getMorpheme.getBasicForm
        if(tok.getMorpheme.getPartOfSpeech.contains("名詞")){
          if(maxmeisival<meisiomomi.get(word).get){
            maxmeisival=meisiomomi.get(word).get
          }
          meisiflag=true
        }
        var check = true
        if (dicyou.contains(word)) {
          val se = dicyou.get(word).get
          var j = 1
          var fil = se
          var loop = true
          if(se.size == 1){
            if(fil.toSeq(0).length==2) {
              va += fil.toSeq(0).last.toInt.toDouble
              count += 1
            }
            check = false
          }
          while (check && loop) {
            if (i + j <= tokens.length - 1) {
              val tt = tokens.get(i + j)
              val wor = if (tt.getMorpheme.getBasicForm == "*") tt.getSurface else tt.getMorpheme.getBasicForm
              fil = fil.filter(s => s(j) == wor || s(j) == tt.getSurface)
              if (fil.size == 1) {
                if (fil.toSeq(0).length == j + 2) {
                  va += fil.toSeq(0).last.toInt.toDouble
                  count += 1
                  check = false
                }
              } else if (fil.size == 0) {
                loop = false
              }
              j += 1
            } else {
              loop = false
            }
          }
          if (check) {
            if (dicmap.contains(word)) {
              count += 1
              val atai = dicmap.getOrElse(word, 0d)
              va += (atai)
              //print(i-mae)
            }

          }
        } else{
          if (dicmap.contains(word)) {
            count += 1
            val atai = dicmap.getOrElse(word,0d)
            va += (atai)
            //print((i-mae))
          }

        }
        va
      }
      println(va+":"+count)
      if(meisiflag) {
        zentairyou+=maxmeisival
        va * maxmeisival
      }else{
        zentairyou+=1
        va
      }
    })
    nnnppp/zentairyou
  }

  def dfuse(listat:List[String],word:String): Map[String,Double] ={//共起？検索単語と一緒に使われやすさを求める
    var couse = Map.empty[String,Double]//名詞の出現ツイート数，目的単語の出現ツイート数，共起ツイート数,ツイート数
    var obuse = 0d
    val ssize=listat.size.toDouble
    val meisi = listat.foldLeft(Map.empty[String,Double])((map,list)=> {//Tscoreを求める準備
      val st = XML.loadFile(list).text
      if(st.contains(word)){
        obuse+=1d
      }
      val tokens = core.tagger.analyze(st,new java.util.ArrayList[Token]())
      var se = Set.empty[String]
      val hh = tokens.foldLeft(map)((minima, minis) => {
        if(minis.getMorpheme.getPartOfSpeech.contains("名詞")){
          if(!se.contains(minis.getSurface)) {
            if(st.contains(word)){
              couse+=(minis.getSurface->(couse.getOrElse(minis.getSurface,0d)+1d/ssize))
            }
            se=se+minis.getSurface
            minima + (minis.getSurface -> (minima.getOrElse(minis.getSurface, 0d) + 1d/ssize))
          }
          else{
            minima
          }
        }
        else{
          minima
        }
      })
      hh
    })
    val maxscore = Math.sqrt(obuse)*(1-(obuse/ssize))
    couse.foldLeft(Map.empty[String,Double])((scoremap,co)=>{
      val score = (co._2-obuse*meisi.getOrElse(co._1,0d)/ssize)/Math.sqrt(co._2)
      val sinscore = if(score>=2){
        score/maxscore
      }else{
        0
      }
      scoremap+(co._1->sinscore)
    })
  }

  def doujiuse(listat:List[Status]): Map[String,Double] ={//共起？検索単語と一緒に使われやすさを求める
    val aaa = listat.foldLeft(Map.empty[String,Double])((map,st)=> {
      val tokens = core.tagger.analyze(st.getText,new java.util.ArrayList[Token]())
      var se = Set.empty[String]
      val hh = tokens.foldLeft(map)((minima, minis) => {
        if(minis.getMorpheme.getPartOfSpeech.contains("名詞")){
          if(!se.contains(minis.getSurface)) {
            se=se+minis.getSurface
            minima + (minis.getSurface -> (minima.getOrElse(minis.getSurface, 0d) + 1d/listat.length))
          }
          else{
            minima
          }
        }
        else{
          minima
        }
      })
      hh
    })
    aaa
  }

  def tfidf(tf:Map[String,Int],idf:Map[String,Int]): Map[String,Double] ={
    tf.foldLeft(Map.empty[String,Double])((mal,ttt)=>{
      val idv = idf.getOrElse(ttt._1,2)
      mal+(ttt._1->(ttt._2/idv.toDouble))
    })
  }

  def read_kaomoji(urls:String): Unit ={
    val svc = url("http://yakata.if.tv/pc/kao/warau.html")
    val country = Http(svc OK as.String)
    var x =""
    country.onSuccess({
      case str:String=> x=str
    } )
    println(x)
  }
  /*def toNode(str:String): Node = {
    val hp = new HtmlParser
    hp.setNamePolicy(XmlViolationPolicy.ALLOW)

    val saxer = new NoBindingFactoryAdapter
    hp.setContentHandler(saxer)
    hp.parse(new InputSource(new StringReader(str)))

    saxer.rootElem
  }*/
}
