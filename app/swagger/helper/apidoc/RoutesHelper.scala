package swagger.helper.apidoc



case class RouteEntry(restMethod: String, uri: String, scalaClass: String, scalaMethod: String)

object RoutesHelper{

  import play.api.Play.current

  // code in this method copied from the swagger play2 module.
  private def getRestClassName(annoDocField3: String) = {
    val m1 = annoDocField3.lastIndexOf("(") match {
      case i: Int if (i > 0) => annoDocField3.substring(0, i)
      case _ => annoDocField3
    }
    m1.substring(0, m1.lastIndexOf(".")).replace("@", "")
  }

  //controllers.ApiDocController.get -> get
  //controllers.ApiDocController.get(path:String) -> get
  private def getRestMethodName(annoDocField3: String) =
    if (annoDocField3.endsWith(")")) {
      val last = annoDocField3.lastIndexOf("(")
      val s2 = annoDocField3.take(last)
      val secondLast = s2.lastIndexOf(".")
      s2.drop(secondLast+1)
    } else {
      val last = annoDocField3.lastIndexOf(".")
      annoDocField3.drop(last+1)
    }


  def getRouteEntries(): List[RouteEntry] =
    play.api.Play.routes.get.documentation.map(doc =>
      RouteEntry(doc._1, doc._2, getRestClassName(doc._3), getRestMethodName(doc._3))
    ).filter(routeEntry =>
      routeEntry.scalaClass != "controllers.Assets" &&
      routeEntry.scalaClass != "controllers.StaticFile" &&
      routeEntry.scalaClass != "controllers.Application"
    ).toList

  // "/api/v1/acl/$service<[^/]+>" -> "/api/v1/acl/{service}"
  def getAutoUriFromConfUri(confUri: String): String =
    if (confUri=="")
      ""
    else if (confUri.startsWith("$")) {
      val pos = confUri.indexOf("<")
      "{" + confUri.substring(1, pos) + "}" + getAutoUriFromConfUri(confUri.drop(pos+7))
    } else
      confUri.take(1) + getAutoUriFromConfUri(confUri.drop(1))

  // /api/v1/50, /api/v1/50 -> true
  // /api/v1/50, /api/v1/{haljapino} -> true
  // /api/v1/50/fields, /api/v1/{haljapino}/fields -> true
  def urisMatches(uri: String, routeUri: String): Boolean = {
    val s_uri      = uri.split("/")
    val s_routeUri = getAutoUriFromConfUri(routeUri).split("/")

    s_uri.size == s_routeUri.size &&
    s_uri.zip(s_routeUri).forall(g => {
      val uri_element = g._1
      val route_element = g._2
      uri_element == route_element || route_element.startsWith("{")
    })
  }

  def findMatchingRouteEntry(
    method: String,
    uri: String,
    routes: List[RouteEntry]
  ): RouteEntry = {

    lazy val route = routes.head

    if(routes.isEmpty)
      throw new Exception("No matching route for "+method + " " + uri)
    else if (method==route.restMethod && urisMatches(uri, route.uri))
      route
    else
      findMatchingRouteEntry(method, uri, routes.tail)
  }
}
