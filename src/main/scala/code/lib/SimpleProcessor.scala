package code.lib

import net.liftweb._
import common.Box
import http.{LiftRules, LiftResponse, Req}
import http.rest._

/**
 * Created with IntelliJ IDEA.
 * User: dasher
 * Date: 1/11/13
 * Time: 3:25 PM
 * To change this template use File | Settings | File Templates.
 */
trait SimpleProcessor extends RestHelper {
  val matchLength:Int
  private var _subHandlers = List.empty[LiftRules.DispatchPF]

  val / = prefix _

  def prefix(pf: LiftRules.DispatchPF): LiftRules.DispatchPF =
  {
    println("prefixing!")
    _subHandlers ::=  pf
    new LiftRules.DispatchPF {
      def isDefinedAt(req: Req): Boolean = this.isDefinedAt(req)
      def apply(req: Req): () => Box[LiftResponse] = this.apply(req)
    }
  }
  override def isDefinedAt(req:Req):Boolean = {
    println("##Processor isDefinedAt called!" + req + "," + this.getClass)
    println("##Processor subHandlers:" + this._subHandlers.length)
    //println("defined in super? " + super.isDefinedAt(req))
    super.isDefinedAt(req) || {
      val req2 = req.withNewPath(req.path.drop(this.matchLength))
      _subHandlers.find(_.isDefinedAt(req2)).isDefined
    }
  }

  override def apply(in: Req): () => Box[LiftResponse] = {
    println("##Processor apply called!")
    val x =  super.apply(in).apply()
    println("##Processor super value:" + x)
    if (x.isDefined) {
      () => x
    }else {
      val req2 = in.withNewPath(in.path.drop(this.matchLength))
      _subHandlers.find(_.isDefinedAt(req2)).get.apply(req2)
    }
  }
}
