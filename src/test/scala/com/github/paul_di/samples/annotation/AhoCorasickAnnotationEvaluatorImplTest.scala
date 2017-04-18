package com.github.paul_di.samples.annotation

import org.scalatest.FunSuite
import scala.io.Source

class AhoCorasickAnnotationEvaluatorImplTest extends FunSuite {
  val evaluator = new AhoCorasickAnnotationEvaluatorImpl(
    Array(
      DictionaryElement(1, "York", Set("Eboracum", "Eburacum", "Eburaci")),
      DictionaryElement(2, "New York", Set("NYC", "New York City")),
      DictionaryElement(3, "City", Set.empty[String])
    )
  )

  test("InlineMatchesExcludedAndScore") {
    val text = "!new york city is the most populous..."
    val calculated = evaluator.annotate(text, MatchSettings(true, false)).toSet
    val correct = Set(AnnotationMatch(2, "New York", "new york city", 1, 14, 0.25))
    assertResult(correct)(calculated)
  }

  test("MusltipleMatchesAndMatchSettings") {
    val text = "York New City Test NYC Test city"
    val calculated = evaluator.annotate(text, MatchSettings(false, true)).toSet
    val correct = Set(
      AnnotationMatch(1, "York", "York", 0, 4, 1.0),
      AnnotationMatch(3, "City", "City", 9, 13, 1.0)
    )
    assertResult(correct)(calculated)
  }


  test("realCase") {
    val evaluator = new AhoCorasickAnnotationEvaluatorImpl(
      dictionary = Source
          .fromInputStream( getClass.getResourceAsStream("/cities15000.txt") )
          .getLines
          .map { line => line.split("\\t") }
          .map { arr => DictionaryElement(arr(0).toLong,
            arr(1).trim, arr(3).split("[,]").map {_.trim }.filter{_.nonEmpty}.toSet) }
          .toArray
    )

    val text =
      """|The earliest known town twinning in Europe was between Paderborn, Germany, and Le Mans, France, in 836.
        |Starting in 1905, Keighley in West Yorkshire, England, had a twinning arrangement with French communities
        |Suresnes and Puteaux. The first recorded modern twinning agreement was between Keighley and Poix-du-Nord in
        |Nord, France, in 1920 following the end of the First World War. This was initially referred to as an adoption
        |of the French town; formal twinning charters were not exchanged until 1986.
        |The practice was continued after the Second World War as a way to promote mutual understanding and
        |cross-border projects of mutual benefit. For example, Coventry twinned with Stalingrad and later with Dresden
        |as an act of peace and reconciliation, all three cities having been heavily bombed during the war.
        |Similarly, in 1947, Bristol Corporation (later Bristol City Council) sent five 'leading citizens' on a goodwill
        |mission to Hanover. Reading in 1947 was the first British town to form links with a former "enemy" city –
        |Düsseldorf. The link still exists (Reading-Düsseldorf Association: http://www.reading-dusseldorf.org.uk/).
        |Since April 9, 1956 Rome and Paris have been exclusively and reciprocally twinned with each other, following
        |the motto: "Only Paris is worthy of Rome; only Rome is worthy of Paris."
        |Within Europe, town twinning is supported by the European Union. The support scheme was established in 1989.
        |In 2003 an annual budget of about €12 million was allocated to about 1,300 projects. The Council of European
        |Municipalities and Regions also works closely with the Commission (DG Education and Culture) to promote modern,
        |high quality twinning initiatives and exchanges that involve all sections of the community. It has launched
        |a website dedicated to town twinning. As of 1995, the European Union had more than 7,000 bilateral relationships
        |involving almost 10,000 European municipalities, primarily French (2837 twinnings) and German (2485 twinnings).
        |Public art has been used to celebrate twin town links, for instance in the form of seven mural paintings in the
        |centre of the town of Sutton, Greater London. The five main paintings show a number of the main features of the
        |London Borough of Sutton and its four twin towns, along with the heraldic shield of each above the other images.
        |Each painting also features a plant as a visual representation of its town's environmental awareness. In the
        |case of Sutton this is in a separate smaller painting (above its main one) showing a beech tree, intended as a
        |symbol of prosperity and from which Carshalton Beeches in the borough derives its name.""".stripMargin

    var annotations = evaluator.annotate("111", MatchSettings(true, false))
    annotations.foreach(println)
    //text = "The earliest known town twinning in Europe was between Paderborn, Germany, and Le Mans, France, in 836"
    println("was")
    val time = System.currentTimeMillis()
    annotations = evaluator.annotate(text, MatchSettings(false, true))
    annotations.foreach(println)
    println("time" + (System.currentTimeMillis() - time))
    println("count" + annotations.length)

  }
}
