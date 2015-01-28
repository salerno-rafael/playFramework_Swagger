package swagger.helper.apidoc

import play.api.libs.json._

object SwaggerUtil{

  val header = Json.obj(
    "apiVersion" -> "1",
    "swaggerVersion" -> "1.2",
    "basePath" -> "",
    "produces" -> Json.arr("application/json","application/xml")
  )

  private def jsonWithoutUndefined(json: JsObject) = JsObject(
    json.value.toSeq.filter(_ match{
      case (name, attributes) => ! attributes.isInstanceOf[JsUndefined]
    })
  )

  // /api/v1/users/     -> /users
  // /api/v1/users/{id} -> /users
  // /api/v1/users      -> /users
  def getResourcePath(uri: String): String = {
    val last = uri.lastIndexOf('/')
    val secondLast = uri.substring(0,last).lastIndexOf('/')

    if (uri.endsWith("/"))
      uri.substring(secondLast,last)

    else if (uri.endsWith("}"))
      uri.substring(secondLast,last)

    else
      uri.drop(last)
  }

  // /api/v1/users/habla/happ  -> /users
  // /api/v1/users/habla/{id}  -> /users
  // /api/v1/users/{id}        -> /users
  // /api/v1/users             -> /users
  def getResourcePathGroup(basePath: String, uri: String): String = {
    val fullPathTail = uri.drop(basePath.length)
    val firstSlash = fullPathTail.indexOf('/')
    if (firstSlash == -1)
      fullPathTail
    else
      fullPathTail.take(firstSlash)
  }

  def getApi(apidoc: JsObject): JsObject =
    Json.obj(
      "path" -> ("/../../../.." + (apidoc \ "uri").as[String]),
      "operations" -> Json.arr(
        Json.obj(
          "method" -> apidoc \ "method",
          "summary" -> apidoc \ "shortDescription",
          "notes" -> apidoc \ "longDescription",
          "type"  -> (if (apidoc.keys.contains("result")) (apidoc \ "result" \ "type") else "void"),
          "nickname" -> "",
          "parameters" -> JsArray(
            if (apidoc.keys.contains("parameters"))
              (apidoc \ "parameters").asInstanceOf[JsObject].value.toSeq.map(_ match{
                  case (name,attributes) => jsonWithoutUndefined(Json.obj(
                    "name" -> name,
                    "type" -> attributes \ "type",
                    "description" -> attributes \ "comment",
                    "paramType" -> attributes \ "paramType",
                    "required" -> (attributes \ "required")
                  ))
              })
            else
              List()
          ),
          "responseMessages" -> apidoc \ "errors"
        )
      )
    )

  def getModels(datatypes: JsObject): JsObject =
    Json.obj(
      "models" -> JsObject(
        datatypes.value.toSeq.map(_ match{
          case (name, attributes) => name -> Json.obj(
            "id" -> name,
            "properties" -> JsObject(
              attributes.asInstanceOf[JsObject].value.toSeq.map(_ match{
                case (name, attributes) => name -> jsonWithoutUndefined(
                  if ((attributes \ "isArray").asInstanceOf[JsBoolean].value)
                    Json.obj(
                      "type" -> "array",
                      "items" -> Json.obj(
                        "$ref" -> attributes \ "type"
                      ),
                      "description" -> attributes \ "comment",
                      "required" -> true
                    )
                  else
                    Json.obj(
                      "type" -> attributes \ "type",
                      "description" -> attributes \ "comment",
                      "required" -> true
                    )
                )
              })
            )
          )
        })
      )
    )


  /*
   "/api/v1/", List("/api/v1/a", "/api/v1/a/c", "/api/v1/b") -> List("a,b")
   */
  def allResourcePathGroups(basePath: String, apidocs: List[String]): Set[String] = {
    val ret =apidocs.map(
      ApiDocUtil.getJson(_)
    ).map(jsonApiDoc =>
      (jsonApiDoc \ "uri").as[String]
    ).map(
      getResourcePathGroup(basePath, _)
    ).toSet
    ret
  }

  private def flattenJsObjects(objs: List[JsObject]): JsObject =
    if (objs.isEmpty)
      Json.obj()
    else
      objs.head ++ flattenJsObjects(objs.tail)

  private def validateUniqueDataTypes(dataTypes: List[JsObject]) = {
    val names = dataTypes.flatMap(_.keys)
    if (names.size != names.distinct.size){
      throw new Exception("One or more ApiDoc datatypes defined more than once: "+names.diff(names.distinct).take(4))
    }
  }

  // {User -> {id -> {type -> String}}} -> Set(String)
  private def getUsedDatatypesInDatatypes(dataTypes: List[JsObject]): Set[String] = {
    if (dataTypes.isEmpty)
      Set()
    else {
      val dataType = dataTypes.head
      val dataTypeValues = dataType.values.map(_.as[JsObject]).flatMap(_.values)
      val subTypes = dataTypeValues.map(o => (o \ "type")).map(_.as[String]).toSet
      subTypes ++ getUsedDatatypesInDatatypes(dataTypes.tail)
    }
  }

  private def getUsedDatatypesInJson(jsonApiDocs: List[JsObject]): Set[String] = {
    if (jsonApiDocs.isEmpty)
      Set()
    else {
      val json = jsonApiDocs.head
      val parameterTypes = if (json.keys.contains("parameters"))
                              (json\"parameters").as[JsObject].values.map(o => (o\"type").as[String]).toSet
                           else
                              Set()
      val returnType = if (json.keys.contains("result"))
                          Set((json\"result"\"type").as[String])
                       else
                         Set()

      parameterTypes ++ returnType ++ getUsedDatatypesInJson(jsonApiDocs.tail)
    }
  }

  private def validateThatAllDatatypesAreDefined(resourcePathGroup: String, jsonApiDocs: List[JsObject], dataTypes: List[JsObject]): Unit = {
    val definedTypes: Set[String] = dataTypes.flatMap(_.keys).toSet ++ ApiDocUtil.atomTypes
    val usedTypes: Set[String]    = getUsedDatatypesInDatatypes(dataTypes) ++ getUsedDatatypesInJson(jsonApiDocs)
    val undefinedTypes            = usedTypes -- definedTypes

    if (undefinedTypes.size>0)
      throw new Exception(s"""${undefinedTypes.size} ApiDoc datatype(s) was/were undefined while evaluating "$resourcePathGroup": """+undefinedTypes.toList.sorted.map(s => s""""$s"""").toString.drop(4))
  }

  def getJson(basePath: String, apidocs: List[String], resourcePathGroup: String): JsObject = {
    val jsonApiDocs = apidocs.map(
      ApiDocUtil.getJson(_)
    ).filter(jsonApiDoc =>
      getResourcePathGroup(basePath, (jsonApiDoc \ "uri").as[String]) == resourcePathGroup
    )

    val dataTypes = apidocs.map(ApiDocUtil.getDataTypes(_))
    validateUniqueDataTypes(dataTypes)

    validateThatAllDatatypesAreDefined(resourcePathGroup, jsonApiDocs, dataTypes)

    header ++
    Json.obj(
      "resourcePath" -> s"/$resourcePathGroup",
      "apis"         -> JsArray(jsonApiDocs.map(getApi(_)))
    ) ++
    getModels(flattenJsObjects(dataTypes))
  }

  def getJson(basePath: String, apidoc: String): JsObject = {
    val jsonApiDoc = ApiDocUtil.getJson(apidoc)
    val resourcePathGroup = getResourcePathGroup(basePath, (jsonApiDoc \ "uri").as[String])
    getJson(basePath, List(apidoc), resourcePathGroup)
  }

  def getMain(basePath: String, apidocs: List[String]): JsObject = {

    header ++
    Json.obj(
      "info" -> Json.obj(
        "title" -> "Rsi Api",
        "description" -> "This is the Retail Sales Incentives  API"
      ),
      "apis" -> JsArray(
        allResourcePathGroups(basePath, apidocs).toList.sorted.map(resourcePathGroup => {
          Json.obj(
            "path" -> s"/$resourcePathGroup",
            "description" -> s"Operations on $resourcePathGroup")
        })
      )
    )
  }
}
