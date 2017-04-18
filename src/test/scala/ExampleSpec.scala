package com.example

import com.github.paul_di.samples.annotation.AnnotationApp
import org.specs2.mutable.Specification
import okhttp3._

object ExampleSpec extends Specification with unfiltered.specs2.jetty.Served {

  override def setup() = { _.plan(new AnnotationApp) }

  val http = new OkHttpClient()

  "The example app" should {
    "serve unfiltered requests" in {
      val request = new Request.Builder().url(host).build()
      val response = http.newCall(request).execute()
      response.code must_== 200
    }
  }
}
