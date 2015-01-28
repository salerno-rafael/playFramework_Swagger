package controllers


import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current

import swagger.helper.apidoc.{ApiDoc, ApiDocUtil, SwaggerUtil}
import swagger.helper.apidoc.{RoutesHelper, RouteEntry}


class ApiDocController extends Controller {


  private val AccessControlAllowOrigin = ("Access-Control-Allow-Origin", "*")

  def hasSameUri(autoUri: String, confUri: String): Boolean = {
    val autos = autoUri.split("/")
    val confs = RoutesHelper.getAutoUriFromConfUri(confUri).split("/")

    autos.size == confs.size &&
    autos.zip(confs).forall(g => {
      val auto = g._1
      val conf = g._2
      auto == conf
    })
  }

  class MethodMismatchException(val message: String) extends Exception(message)
  class UriMismatchException(val message: String) extends Exception(message)

  def hasMethodAnnotation(className: String, methodName: String) = {
    val class_ = play.api.Play.classloader.loadClass(className)
    class_.getDeclaredMethods().find(_.getName()==methodName) match {
      case None => false
      case Some(method) => {
        val annotations = method.getAnnotations()
        annotations.find(_.isInstanceOf[swagger.helper.apidoc.ApiDoc]) != None
      }
    }
  }

  def getMethodAnnotation(className: String, methodName: String) = {
    val class_ = play.api.Play.classloader.loadClass(className)
    class_.getDeclaredMethods().find(
      _.getName()==methodName
    ).get.getAnnotations().find(
      _.isInstanceOf[swagger.helper.apidoc.ApiDoc]
    ).get.asInstanceOf[swagger.helper.apidoc.ApiDoc]
  }

  def validate(routeEntries: List[RouteEntry]): Unit = {
    routeEntries.foreach(routeEntry => {

      if (!hasMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod))
        throw new Exception(s"Missing ApiDoc for ${routeEntry.scalaClass}.${routeEntry.scalaMethod} (Make sure the Class is annotated, and not the corresponding Object)")

      val annotation = getMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod)
      val json = ApiDocUtil.getJson(annotation.doc)
      val jsonMethod = (json\"method").as[String]
      val jsonUri = (json\"uri").as[String]

      if (jsonMethod != routeEntry.restMethod)
        throw new MethodMismatchException(s"Conflicting REST method declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n"+
                                          s"autodoc: $jsonMethod. conf/routes: ${routeEntry.restMethod}")

      if (!hasSameUri(jsonUri, routeEntry.uri))
        throw new UriMismatchException(s"Conflicting uri declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n"+
                                       s"autodoc: $jsonUri. conf/routes: ${routeEntry.uri}")
    })
  }

  lazy val apiDocsFromAnnotations: List[String] = {

    val routeEntries = RoutesHelper.getRouteEntries()

    validate(routeEntries)

    val apiDocAnnotations = routeEntries.map(routeEntry =>
      getMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod)
    )

    val apiDocs = apiDocAnnotations.map(_.doc)

    apiDocs
  }

  @ApiDoc(doc="""
    GET /api/v1/api-docs

    DESCRIPTION
      Get main swagger json documentation
      You can add more detailed information here.
  """)
  def get() = {
    Ok(SwaggerUtil.getMain("/", apiDocsFromAnnotations)).withHeaders(AccessControlAllowOrigin)
  }

  @ApiDoc(doc="""
    GET /api/v1/api-docs/{path}

    DESCRIPTION
      Get swagger documentation json for a resource path
      You can add more detailed information here.

    ERRORS
      404 Path not found

    PARAMETERS
      path: String
  """)
  def getPath(path: String) = {
    Ok(SwaggerUtil.getJson("/", apiDocsFromAnnotations, path)).withHeaders(AccessControlAllowOrigin)
  }
}

object ApiDocController extends Controller {
  val controller = new ApiDocController

  def get()  = Action { request =>
    controller.get()
  }
  def getPath(path: String)  = Action { request =>
    controller.getPath(path)
  }

  def validate(basePath: String) = {
    val annotations = controller.apiDocsFromAnnotations
    SwaggerUtil.allResourcePathGroups(basePath, annotations).foreach(
      SwaggerUtil.getJson(basePath, annotations, _)
    )
  }
}
