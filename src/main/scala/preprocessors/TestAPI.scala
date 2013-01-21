package preprocessors

import RestMagic._
import net.liftweb.http.{LiftResponse, PlainTextResponse, RequestVar, Req}
import net.liftweb.common.{Box, Full, Empty}

/**
 * Created with IntelliJ IDEA.
 * User: z33m
 * Date: 1/18/13
 * Time: 2:01 PM
 * To change this template use File | Settings | File Templates.
 */
object TestAPI extends RestHelperWithPreProcessors {

  object currentUser extends RequestVar[Option[String]](None)
  object someData extends RequestVar[Option[String]](None)
  object organization extends RequestVar[Option[String]](None)


  ////////// preProcess blocks //////////

  // All the following preProcess blocks will be executed,
  // shortcircuting breaks the chain and returns response directly

  /**
   * Simple Preprocessor, will be executed for every case
   */
  preProcess { req:Req =>
    println("A")
    someData(Full("Some Data"))
    Empty
  }

  /**
   * Check Auth header and set currentuser for every
   * request path that starts with api/, shortcircuits
   * with 401 if no auth header is found
   */
  preProcess { case "api" :: _ AnyReq req =>
    println("B")
    req.header("Authorization").map { auth =>
      currentUser(Full("azeem")) // extract something and put in request var
      Empty
    } getOrElse {
      println("Not Authorized Short Circuiting with 401")
      Full(PlainTextResponse("You are not authenticated", 401))
    }
  }

  /**
   * Extract common parameters and put them into request vars
   */
  preProcess { case List("api", _, "organization", org, _*) Get req =>
    println("C")
    organization(Full(org))
    Empty
  }

  /**
   * Preprocessor that dumps all the request
   */
  preProcess { req:Req =>
    println("D")
    dumpRequestVars()
    Empty
  }

  ////////// preProcess blocks //////////

  // Clean serves, no repeated boiler plate. Wohooo!!

  serve("api" / "v1" / Number prefixes {
    case "organization" :: "hello" :: Nil Get req => {
      dumpRequestVars()
      println("Inside /api/v1/organization/hello")
      PlainTextResponse("Inside /api/v1/organization/hello")
    }

    case "about" :: Nil Post req => {
      dumpRequestVars()
      print("Inside /api/v1/about")
      PlainTextResponse("Welcome to the test API")
    }

    case "some" :: "public" :: "endpoint" :: Nil Get req => {
      dumpRequestVars()
      print("Inside /some/public/endpoint")
      PlainTextResponse("This is some random API endpoint")
    }
  })

  /**
   * Dump all request vars
   */
  def dumpRequestVars() {
    println("------------- RequestVar dump -------------")
    println("currentUser = " + currentUser.is)
    println("someData = " + someData.is)
    println("organization = " + organization.is)
    println("-------------------------------------------")
  }

}
