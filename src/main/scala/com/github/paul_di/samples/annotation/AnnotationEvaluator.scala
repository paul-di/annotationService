package com.github.paul_di.samples.annotation

import java.net.URL
import java.util.zip.ZipInputStream

import opennlp.tools.tokenize.SimpleTokenizer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.io.Source

case class MatchSettings(useAlternativeNames: Boolean, caseSensetive: Boolean)

case class DictionaryElement(geoid: Long, name: String, alternativenames: Set[String])

trait AnnotationEvaluator {
  def annotate(text: String, matchSettings: MatchSettings): Array[AnnotationMatch]
}

object AhoCorasickAnnotationEvaluator extends AhoCorasickAnnotationEvaluatorImpl(
  dictionary = {
    val inputStream = new URL("http://download.geonames.org/export/dump/cities15000.zip").openStream()
    val unzippedStream = new ZipInputStream(inputStream)
    Source
      .fromInputStream(unzippedStream, "UTF-8")
      .getLines
      .map { line => line.split("\\t") }
      .map { arr => DictionaryElement(arr(0).toLong, arr(1).trim, arr(3).split("[,]").map{_.trim}.toSet) }
      .toArray
  }
)

class AhoCorasickAnnotationEvaluatorImpl(val dictionary: Array[DictionaryElement]) extends AnnotationEvaluator {

  //lowercased aho-Corasickk tree with fictive top node
  private lazy val topNode: AhoCorasickNode = {
    val topNode = AhoCorasickNode(0.toChar)
    dictionary.foreach { dictEl => //disabl
      Some(/*dictEl.alternativenames + */dictEl.name).map{ _.toLowerCase }.foreach { term =>
        recursiveTreeBranchBuild(term, topNode).endMarks.add(dictEl)
      }
    }
    topNode
  }

  private lazy val maxTreeDepth: Int = {
    def maxDepth(depth: Int, node: AhoCorasickNode): Int = {
      if(node.children.isEmpty) {
        return depth
      }
      node.children.values.map {maxDepth(depth + 1, _)}.max
    }
    maxDepth(1, topNode)
  }

  // построение ветви дерева разбора для одного терма. Возвращает конечный узел данного терма
  @tailrec private def recursiveTreeBranchBuild(term: String, curNode: AhoCorasickNode): AhoCorasickNode = {
    val nextNode = curNode.children.getOrElseUpdate(term.head, AhoCorasickNode(term.head))
    if(term.tail.isEmpty)
      nextNode
    else
      recursiveTreeBranchBuild(term.tail, nextNode)
  }

  override def annotate(textRaw: String, matchSettings: MatchSettings): Array[AnnotationMatch] = {
    val text = textRaw.replaceAll("\\s", " ")
    val tokenizationSpans = SimpleTokenizer.INSTANCE.tokenizePos(text)
    val tokenStartIndexesInclusive = tokenizationSpans.map { _.getStart }.toSet
    val endIndexesExclusive = tokenizationSpans.map { _.getEnd }.toSet

    val lowercasedText = text.toLowerCase
    var prevStart = 0
    var prevEnd = 0

    //match should start from first symbol of some token
    val foundMatches: List[(Int, Int, SearchRes)] = tokenStartIndexesInclusive.toList.sorted.flatMap { start =>
      var it = lowercasedText.substring(start, Math.min(start + maxTreeDepth + 1, lowercasedText.length)).iterator
      var node = topNode
      var foundStr: Option[String] = Some("")
      val searchResults = mutable.ListBuffer(recursiveSearchInTree(it, node, foundStr.get))

      //recurcive search can return several matched strings (starting from same index)
      while (searchResults.last.foundStr.nonEmpty) {
        val searchRes = searchResults.last
        it = searchRes.it
        node = searchRes.lastNode
        foundStr = searchRes.foundStr
        searchResults.append(recursiveSearchInTree(it, node, foundStr.get))
      }

      searchResults
        .filter { _.foundStr.nonEmpty } //remove empty matches that signaliziing about finish of search
        .filter { sr => endIndexesExclusive.contains(start + sr.foundStr.get.length) } //match should ends at last symbol of some token
        .map{ sr => (start, start + sr.foundStr.get.length, sr)}
        .sortBy { case (start, end, sr) => start - end }//remove matches that laying completely inside previous match
        .filter { case (start, end, sr) => //:TODO that tricky logic can be replaced with search of nested ranges
          if(prevEnd >= prevEnd || (prevEnd == end && prevStart >= start)) {
            prevEnd = end
            prevStart = start
            true
          } else {
            false
          }
        }
    }

    generateAnnotations(foundMatches, text, matchSettings).toArray
  }

  private def generateAnnotations(matches: List[(Int, Int, SearchRes)], text: String, settings: MatchSettings) = {
    matches.flatMap { case (start, end, sr) =>
      sr.dictElements.flatMap { dicktElement =>
        val usedAlternativeNames = dicktElement.name.toLowerCase != sr.foundStr.get.toLowerCase
        val usedCaseSensetiveMatch = if(usedAlternativeNames) {
          dicktElement.alternativenames.contains(text.substring(start, end))
        } else {
          dicktElement.name == text.substring(start, end)
        }

        val score = (if(usedAlternativeNames) 0.5 else 1) * (if(usedCaseSensetiveMatch) 1.0 else 0.5)

        if((!usedAlternativeNames || settings.useAlternativeNames) && (usedCaseSensetiveMatch || !settings.caseSensetive)) {
          Some(AnnotationMatch(dicktElement.geoid, dicktElement.name, text.substring(start, end), start, end, score))
        } else {
          None
        }
      }
    }
  }

  @tailrec private def recursiveSearchInTree(
                                              it: Iterator[Char], //iterator over text
                                              node: AhoCorasickNode, //current node in ahoCorasick tree
                                              foundStr: String //current found substring
                                            ): SearchRes = {

    if(!it.hasNext) {//text is ended. return res with foundStr=None. It indicates that we found nothing
      SearchRes(it, node, None, mutable.HashSet.empty[DictionaryElement])
    } else {

      val curSymbol = it.next//get next text symbol
      if(node.children.contains(curSymbol)) {//check that we have child node in search tree for cur symbol
        val nextNode = node.children(curSymbol)
        val nextStr = foundStr + new String(Array(nextNode.value))//append cur symbol to foundStr

        if(nextNode.endMarks.nonEmpty) {//endMark indicates that we successfully found some term entry. return result
          SearchRes(it, nextNode, Some(nextStr), nextNode.endMarks)//but we still should try continue search from that place
        } else {//no endMarks. continue search recursively
          recursiveSearchInTree(it, nextNode, nextStr)
        }
      } else {//there is no child node for cur symbol. return res with foundStr=None
        SearchRes(it, node, None, mutable.HashSet.empty[DictionaryElement])
      }
    }
  }
}

private case class SearchRes(
  it: Iterator[Char],
  lastNode: AhoCorasickNode,
  foundStr: Option[String],
  dictElements: mutable.Set[DictionaryElement]
)

private case class AhoCorasickNode(
  value: Char,
  children: mutable.HashMap[Char,AhoCorasickNode] = mutable.HashMap.empty[Char,AhoCorasickNode],
  endMarks: mutable.HashSet[DictionaryElement] = mutable.HashSet.empty[DictionaryElement]
)