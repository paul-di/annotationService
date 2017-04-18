package com.github.paul_di.samples.annotation

case class AnnotationMatch(
  /** id of term from dictionary  */
  geonameid: Long,

  /** term name from dictionary */
  name: String,

  /** text fragment that was matched with term */
  covered_text: String,

  /** start index of matched text fragment at original text (inclusive) */
  start: Int,

  /** end index of matched text fragment at original text (inclusive) */
  end: Int,

  /** mystical */
  score: Double
) {}
