package code.api

import net.liftweb._
import common.{Empty, Box, Full}
import http._
import rest._

import code.lib.RestAddons._

/**
 * Created with IntelliJ IDEA.
 * User: dasher
 * Date: 1/11/13
 * Time: 4:46 PM
 * To change this template use File | Settings | File Templates.
 */
object Processor1 extends ProtectedHelper with common.Loggable {
  protect {
    case Req(List("p1",_*), _, _) => println("Processor1:p1");Full(PlainTextResponse("This ain't P1"))
    case Req(List("p2",_*), _, _) => println("Processor1:p2");Empty
  }
  serve{
    case "p1":: Nil Get _ =>
    logger.info("p1 get Called!")
    OkResponse()
  }
}
object Processor2 extends ProtectedHelper with common.Loggable {
  protect {
    //case Req(List("p2",_*), _, _) => println("Processor2:p2");Full(PlainTextResponse("This ain't P2"))
    case Req(List("p1",_*), _, _) => println("Processor2:p1");Empty
  }
  serve{
    case "p2":: Nil Get _ =>
      logger.info("p2 get Called!")
      OkResponse()
  }
}