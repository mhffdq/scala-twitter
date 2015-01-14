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

  def getnegaposi_gyou(text:String,sea:String) : Double = {//極性をもった単語が同じ行とかそういうのを見る
    val p = text.indexOfSlice(sea)
    val sptexts=text.split("\n")
    var count =0
    val nnnppp = sptexts.foldLeft(0d)((vvv,s)=>{
      val tokens = core.tagger.analyze(s,new java.util.ArrayList[Token]())
      var mae = 0
      var va = vvv
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
        va
      }
      println(va+":"+count)
        va
    })
    nnnppp
  }

  def idfuse(listat:List[String]): Map[String,Double] ={//共起？検索単語と一緒に使われやすさを求める
    val aaa = listat.foldLeft(Map.empty[String,Double])((map,st)=> {
      val tokens = core.tagger.analyze(st,new java.util.ArrayList[Token]())
      var se = Set.empty[String]
      val hh = tokens.foldLeft(map)((minima, minis) => {
        if(minis.getMorpheme.getPartOfSpeech.contains("名詞")){
          if(!se.contains(minis.getSurface)) {
            se=se+minis.getSurface
            minima + (minis.getSurface -> (minima.getOrElse(minis.getSurface, 0d) + 1d/listat.length.toDouble))
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