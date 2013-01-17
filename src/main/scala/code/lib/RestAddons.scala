package code.lib

import net.liftweb._
import common.{Full, Empty, Box}
import http.{LiftRules, LiftResponse, Req}
import http.rest._

/**
 * Created with IntelliJ IDEA.
 * User: dasher
 * Date: 1/11/13
 * Time: 3:25 PM
 * To change this template use File | Settings | File Templates.
 */
object RestAddons {
  type Protector = PartialFunction[Req,Box[LiftResponse]]

  trait ProtectedHelper extends RestHelper {
    @volatile private var _protector: Protector= null

    def protect( by: Protector) { _protector = if(_protector == null) by else _protector orElse by }

    override def isDefinedAt(in: Req): Boolean = super.isDefinedAt(in)

    override def apply(in: Req): () => Box[LiftResponse] = {
      val res = if(_protector.isDefinedAt(in)){
        _protector.apply(in)
      } else Empty

      res.map(r => (() => res)).openOr(super.apply(in))
    }
  }

}