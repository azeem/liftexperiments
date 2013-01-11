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
  var _subHandlers = List.empty[LiftRules.DispatchPF]
  val self = this

  val / = prefix _

  def prefix(pf: LiftRules.DispatchPF): LiftRules.DispatchPF =
  {
    //
    _subHandlers =  pf :: _subHandlers
    new LiftRules.DispatchPF {
      def isDefinedAt(req: Req): Boolean = self.isDefinedAt(req)
        //req.path.partPath.startsWith(list) && {
        //  pf.isDefinedAt(req.withNewPath(req.path.drop(listLen)))
        //}

      def apply(req: Req): () => Box[LiftResponse] = self.apply(req)
        //pf.apply(req.withNewPath(req.path.drop(listLen)))
    }
  }
  override def isDefinedAt(req:Req):Boolean = {
    super.isDefinedAt(req) && _subHandlers.find(_.isDefinedAt(req)).isDefined
  }

  override def apply(in: Req): () => Box[LiftResponse] = {
    val x =  super.apply(in).apply()
    if (x.isDefined) {
      () => x
    }else _subHandlers.find(_.isDefinedAt(in)).get.apply(in)
  }
}
