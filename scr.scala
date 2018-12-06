/**
  * Created by agoetc on 2016/10/28.
  */

import org.chasen.mecab._
import org.jsoup._
import org.jsoup.nodes.Document

import scala.collection.mutable
import scala.collection.mutable.MutableList

class getURL (goo:String){

  //URL取得
  def getURL: String = {

    //URLの中身を収納
      val doc: Document = Jsoup.connect("https://search.yahoo.co.jp/search?&p=" + goo).get()

    val text: String = doc.body().text()
    println()

    return text
  }
  //mecabで解析
  def node(text: String):(mutable.HashMap[Int,String],mutable.HashMap[String,Int])={
    System.loadLibrary("MeCab")

    val tagger: Tagger = new Tagger()
    val text: String = getURL
    var node = tagger.parseToNode(text)

    var term: String = ""
    var count = 0
    var Number = 1

    val mapN = new mutable.HashMap[Int,String]
    val mapC = new mutable.HashMap[String,Int]

    while (node != null) {


      var splited = node.getFeature.split(',')
      if (splited(0) == ("名詞")) {

        //文字列を入れる
        term = splited(6)

        //いらない文字を省く
        term = term.replaceAll("日本","")
        term = term.replaceAll("語","")
        term = term.replaceAll("検索","")
        term = term.replaceAll("ページ","")
        term = term.replaceAll("情報","")
        term = term.replaceAll("人気","")
        term = term.replaceAll("辞典","")
        term = term.replaceAll("言い方","")
        term = term.replaceAll("類","")
        term = term.replaceAll("同義","")
        term = term.replaceAll("年","")
        term = term.replace("*","")

        term = term.replaceAll("Rights","＿")



        term = term.replace(goo, "")



        //空白の場合次の文字へ
        if (term.isEmpty() == true) {
          node = node.getNext
        }
        if(term.forall(_.isDigit) == true){
          node = node.getNext
        }else{
            //マップに存在する場合+=1
          if (mapC.contains(term) == true) {
            count = mapC.apply(term)
            count += 1
            mapC.put(term, count)
            //ない場合新しくmapに追加
          } else {
            count = 0
            mapC.put(term, count)
            mapN.put(Number,term)
            Number += 1

          }
        }
      }
      node = node.getNext
    }

    val Rmap = (mapN,mapC)

    return Rmap
  }
  //出現回数上位３つを取得
  def moji(mapN:mutable.HashMap[Int,String], mapC:mutable.HashMap[String,Int]):(mutable.MutableList[String],mutable.MutableList[Int])= {
    //mapの添字
    var count = 0

    //文字の出現した回数
    val kazu = 2

    val ranki = MutableList(0, 0, 0, 0, 0)
    val ranks = MutableList("", "", "", "", "")

    //出現回数ランキングベスト３
    val rank = new mutable.HashMap[Int, String]
    //キーの数だけ繰り返す
    mapC.foreach { case (key, value) =>

      count+=1

      //添字のvaleuを取り出す
      //そのvaleuをmapCのkeyにする
      val mapCK = mapN.apply(count)
      val mapCV = mapC.apply(mapCK)

      //出現回数が上位１の場合
      if (ranki(0) <= mapCV) {
        ranki.update(0, mapCV)
        ranks.update(0, mapCK)
      }
      else if (ranki(1) <= mapCV) {
        ranki.update(1, mapCV)
        ranks.update(1, mapCK)
      }
      else if (ranki(2) <= mapCV) {
        ranki.update(2, mapCV)
        ranks.update(2, mapCK)
      }
      else if (ranki(3) <= mapCV) {
        ranki.update(3, mapCV)
        ranks.update(3, mapCK)
      }
      else if (ranki(4) <= mapCV) {
        ranki.update(4, mapCV)
        ranks.update(4, mapCK)
      }
    }

    return (ranks,ranki)
  }

}



object main {


  println("といえば検索")
  print("日本語を入力！：")
  val goo = io.StdIn.readLine()
  val get = new getURL(goo)

  def main(args: Array[String]): Unit = {
    val text: String = get.getURL
    val node = get.node(text)
    val moji = get.moji(node._1,node._2)

    println("1" + moji._1(0) + moji._2(0))
    println("2" + moji._1(1) + moji._2(1))
    println("3" + moji._1(2) + moji._2(2))
    println("4" + moji._1(3) + moji._2(3))
    println("5" + moji._1(4) + moji._2(4))
  }
}
