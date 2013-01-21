package preprocessors

import net.liftweb.http.{Req, LiftResponse}
import net.liftweb.common.{Empty, Full, Box}
import net.liftweb.http.rest.RestHelper
import scala.util.control.Breaks._
import util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: z33m
 * Date: 1/18/13
 * Time: 1:50 PM
 * To change this template use File | Settings | File Templates.
 */
object RestMagic {
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
    def preProcess(pp: PreProcessor) { preprocessors = preprocessors ::: List(pp) }

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
      val shortCircuit = preprocessors.filter(_.isDefinedAt(req)).iterator.map(_.apply(req)).find(_.isDefined)
      shortCircuit.map(r => () => r).getOrElse(super.apply(req))
    }
  }


  val Number = "\\d+"
  val AlphaNum = "\\w+"
  val Hex = "[a-fA-F0-9]+"
  val Hash = "[a-fA-F0-9\\-]+"

  class PrefixMagic(list: List[String]) {
    val len = list.length
    val elems = list.map(str => new Regex(str))

    def prefixMatch(target: List[String]) = {
      if(target.length >= len) {
        target.take(len).zip(elems).forall { case (str, reg) =>
          reg.pattern.matcher(str).matches()
        }
      } else false
    }

    def prefixes(pf: PartialFunction[Req, ()=>Box[LiftResponse]]): PartialFunction[Req, ()=>Box[LiftResponse]] = {
      new PartialFunction[Req, ()=>Box[LiftResponse]] {
        def isDefinedAt(req: Req) = {
          prefixMatch(req.path.partPath) && pf.isDefinedAt(req.withNewPath(req.path.drop(len)))
        }

        def apply(req: Req) = pf.apply(req.withNewPath(req.path.drop(len)))
      }
    }
  }

  implicit def listToPrefixMagic(list: List[String]) = new PrefixMagic(list)
}
