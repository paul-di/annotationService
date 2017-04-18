package com.github.paul_di.samples.annotation

import unfiltered.directives._
import unfiltered.request._
import unfiltered.response._

//** unfiltered plan */
class AnnotationApp extends unfiltered.filter.Plan {

  def intent = Directive.Intent {
    case GET(r) =>
      null
  }
}

object Server {
  def main(args: Array[String]) = {
    unfiltered.jetty.Server.local(4567).anylocal.plan(new AnnotationApp).run()
  }
}
