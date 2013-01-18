package preprocessors

import net.liftweb.http.{Req, LiftResponse}
import net.liftweb.common.{Empty, Full, Box}
import net.liftweb.http.rest.RestHelper
import scala.util.control.Breaks._

/**
 * Created with IntelliJ IDEA.
 * User: z33m
 * Date: 1/18/13
 * Time: 1:50 PM
 * To change this template use File | Settings | File Templates.
 */
object PreProcessors {
  /**
   * Preprocessor is a partial function from Req to Box[LiftResponse]
   * it can be used to
   * 1. do any initialization or pre-processing before any of the serve blocks are executed
   * 2. short circuit any of the serve blocks and return a LiftResponse directly
   */
  type PreProcessor = PartialFunction[Req, Box[LiftResponse]]

  trait RestHelperWithPreProcessors extends RestHelper {
    @volatile private var preprocessors: List[PreProcessor] = Nil // list of preprocessors

    /**
     * An extractor that extracts the path and the request
     */
    protected object AnyReq {
      def unapply(r: Req): Option[(List[String], Req)] = Some((r.path.partPath, r))
    }

    /**
     * Define a preprocessor
*/
    def preProcess(pp: PreProcessor) { preprocessors ::= pp }

    /**
     * Implicit that converts simple functions to preprocessor partial functions
     */
    implicit def funcToPreProcessor(f: (Req) => Box[LiftResponse]): PreProcessor = {
      new PreProcessor {
        def isDefinedAt(req: Req) = true
        def apply(req: Req) = f(req)
      }
    }

    /**
     * Overridden RestHelper apply. This function Runs every
     * preprocessor IN ORDER. If any one preprocessor returns
     * a Full(LiftResponse), then the execution short circuits
     * and the LiftResponse is sent instead executing the serve blocks
     */
    override def apply(req: Req) = {
      var shortCircuit: Box[LiftResponse] = Empty
      breakable {
        preprocessors.filter(_.isDefinedAt(req)).foreach { pp =>
          val resp = pp.apply(req)
          if(resp.isDefined) {
            shortCircuit = resp
            break
          }
        }
      }
      shortCircuit.map(r => () => Full(r)).getOrElse(super.apply(req))
    }
  }
}
