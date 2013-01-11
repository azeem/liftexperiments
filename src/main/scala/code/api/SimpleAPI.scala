package code.api

import net.liftweb._
import common.Full
import http._
import rest._

import code.lib.SimpleProcessor

/**
 * Created with IntelliJ IDEA.
 * User: dasher
 * Date: 1/11/13
 * Time: 4:46 PM
 * To change this template use File | Settings | File Templates.
 */
object Processor1 extends SimpleProcessor with common.Loggable {
  val matchLength = ("p1" :: Nil).length
  serve{
    case "p1":: Nil Get _ =>
    logger.info("p1 get Called!")
    OkResponse()
  }
}
object Processor2 extends SimpleProcessor with common.Loggable {
  val matchLength = ("p2" :: Nil).length
  serve{
    case "p2":: Nil Get _ =>
      logger.info("p2 get Called!")
      OkResponse()
  }
}

object SimpleAPI extends RestHelper with common.Loggable {
  println("working")
  serve (Processor1 prefix {
    case "simple" :: Nil Get _ =>
    logger.info("simple got called!")
    PlainTextResponse("simple got  called!")
  })
}
