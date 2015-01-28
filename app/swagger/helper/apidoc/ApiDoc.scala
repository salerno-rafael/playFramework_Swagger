package swagger.helper.apidoc



import play.api.Play.current
import play.api.libs.json._
import com.fasterxml.jackson.annotation.JsonIgnore

object ApiDocUtil{

  val atomTypes = Set("etc.", "String", "Long", "Boolean", "Integer", "Int", "Any", "Double", "Float","List[view.model.DiscountOutput]")

  class MismatchFieldException(message: String) extends Exception(message)
  class UnknownFieldException(message: String) extends Exception(message)
  class AlreadyDefinedFieldException(message: String) extends Exception(message)
  class MismatchPathParametersException(message: String) extends Exception(message)

  private def safeLoadClass(className: String): java.lang.Class[_] =
    try{
      play.api.Play.classloader.loadClass(className)
     } catch {
        case e: java.lang.ClassNotFoundException => null.asInstanceOf[java.lang.Class[_]]
    }
 
  private def loadInnerClass(parent: java.lang.Class[_], className: String, elms: List[String]): java.lang.Class[_] = {

    if (parent==null && elms.isEmpty) {
      play.api.Play.classloader.loadClass(className)

    } else if (parent==null) {
      val class_ = safeLoadClass(elms(0))
      if (class_ != null)
        loadInnerClass(class_, className, elms.tail)
      else if (elms.size==1)
        play.api.Play.classloader.loadClass(className)
      else
        loadInnerClass(null, className, elms(0)+"."+elms(1) :: elms.tail.tail)

    } else if (elms.isEmpty) {
      parent

    } else {
      val class_ = parent.getClasses.find(_.getCanonicalName()==parent.getCanonicalName()+"."+elms(0))
      if (class_ != None)
        loadInnerClass(class_.get, className, elms.tail)
      else
        play.api.Play.classloader.loadClass(className)
    }
  }

  def loadInnerClass(className: String): java.lang.Class[_] =
    loadInnerClass(null.asInstanceOf[java.lang.Class[_]], className, className.split('.').toList)



  def findUriParm(autoUri: String): List[String] =
    if (autoUri=="")
      List()
    else if (autoUri.startsWith("{")) {
      val next = autoUri.indexOf('}')
      autoUri.substring(1, next) :: findUriParm(autoUri.drop(next+1))
    } else
      findUriParm(autoUri.drop(1))



  def validateDataTypeFields(className: String, dataTypeName: String, fields: Set[String], addedFields: Set[String], removedFields: Set[String]): Unit = {

    val class_ = try{
      loadInnerClass(className)
    } catch {
      case e: java.lang.ClassNotFoundException =>
        loadInnerClass("models."+className)
    }

    def getClassFieldNames(parameterAnnotations: List[Array[java.lang.annotation.Annotation]], fields: List[java.lang.reflect.Field]): List[String] = {
      if (fields.isEmpty)
        List()
      else {
        val field = fields.head
        val annotations = if (parameterAnnotations.isEmpty) field.getDeclaredAnnotations().toList else parameterAnnotations.head.toList
        val rest = getClassFieldNames(parameterAnnotations.drop(1), fields.tail) // List().drop(1) == List()

        val fieldName = field.getName()
        if(fieldName.contains("$"))
          rest
        else
          annotations.find(_.isInstanceOf[JsonIgnore]) match {
            case Some(annotation: JsonIgnore) if annotation.value==true => rest
            case None => fieldName :: rest
          }
      }
    }

    if(class_.getConstructors.isEmpty)
      throw new Exception(s"""While evaluating "${dataTypeName}": Class $class_ does not have any constructors.""")

    val classFields = getClassFieldNames(
      class_.getConstructors.head.getParameterAnnotations().toList,
      class_.getDeclaredFields().toList
    ).toSet

    if ( (removedFields &~ classFields).size > 0)
      throw new UnknownFieldException(s"""While evaluating "${dataTypeName}": One or more removedFields are not defined for class '$className': """ + (removedFields &~ classFields) + ". classFields: "+classFields)

    if ( (addedFields & classFields).size > 0)
      throw new AlreadyDefinedFieldException(s"""While evaluating "${dataTypeName}": One or more addedFields are already defined for class '$className': """+(addedFields & classFields))

    if ( (addedFields & removedFields).size > 0)
      throw new AlreadyDefinedFieldException(s"""While evaluating "${dataTypeName}": One or more fields are both present in addedFields and removedFields (for '$className'): """+(addedFields & removedFields))

    val modifiedClassFields = classFields ++ addedFields -- removedFields

    if ( fields != modifiedClassFields)
      throw new MismatchFieldException(s"""While evaluating "${dataTypeName}": The ApiDoc datatype does not match the class '$className'. Mismatched fields: """+ ((fields | modifiedClassFields) -- (fields & modifiedClassFields)))
  }

  private def getIndentLength(line: String) =
    line.prefixLength(_==' ')

  case class Raw(key: String, elements: List[String]) {
    def plus(element: String) =
      Raw(key, elements ++ List(element))

    case class TypeInfo(val parmName: String, val typetypetype: String){                             // typetypetype = "Array String (header)"
      val isArray     = typetypetype.startsWith("Array")
      val typetype    = if (isArray) typetypetype.drop(6).trim else typetypetype                     // typetype = "String (header)"
      val hasParmType = typetype.endsWith(")")
      val firstSpace  = typetype.indexOf(' ')
      val type_       = if (hasParmType) typetype.take(firstSpace).trim else typetype.trim           // type_ = "String"                           
      var paramType    = if (hasParmType)                                                            // paramType = "header"
                           typetype.drop(firstSpace).dropWhile(_!='(').drop(1).dropRight(1).trim 
                        else if (parmName=="body")
                           "body"
                        else
                           "path"
      val optional   = if (hasParmType==false)
                         false
                       else if (!paramType.contains(','))
                         false
                       else 
                         (paramType.split(',')(1)==" optional")

      paramType = paramType.split(',')(0) // "query, optional" -> "query"

      if ( ! Set("body", "path", "query", "header", "form").contains(paramType))
        throw new Exception(s""""$paramType" is not a valid paramameter type. It must be either "body", "path", "query", "header", or "form". See https://github.com/wordnik/swagger-core/wiki/Parameters""")
    }

    private def getParameters(): JsObject =
      JsObject(
        elements.map(element => {
          if (element=="...")
            "..." -> Json.obj(
              "type" -> "etc.",
              "isArray" -> false
            )
          else {
            val nameLength = element.indexOf(':', 0)
            if(nameLength == -1)
              throw new Exception(s"Syntax error for element '$element'. (Missing ':')")
            val name = element.substring(0,nameLength)
            val rest = element.drop(nameLength+1).trim.split("<-")
            val typeInfo = TypeInfo(name, rest(0).trim)
            val comment = if (rest.length==1) "" else rest(1).trim

            name -> Json.obj(
              "type" -> typeInfo.type_,
              if (comment=="")
                "noComment" -> true
              else
                "comment" -> comment,
              "isArray" -> typeInfo.isArray,
              "paramType" -> typeInfo.paramType,
              "required"  -> !typeInfo.optional
            )
          }
        })
      )

    private def parseScalaTypeSignature(signature: String): (String, Set[String], Set[String]) = {

      val leftParPos = signature.indexOf('(')
      val rightParPos = signature.indexOf(')')

      if(leftParPos== -1 && rightParPos!= -1)
        throw new Exception("Malformed line: "+signature)
      if(leftParPos!= -1 && rightParPos== -1)
        throw new Exception("Malformed line: "+signature)
      if(leftParPos > rightParPos)
        throw new Exception("Malformed line: "+signature)

      if(leftParPos == -1) {

        (signature, Set(), Set())

      } else {

        val className = signature.take(leftParPos).trim
        val argsString = signature.substring(leftParPos+1, rightParPos).trim

        if (argsString=="") {

          (className, Set(), Set())

        } else {
          val modifiedFields = argsString.split(",").toList.map(_.trim)

          val addedFields = modifiedFields.filter(_.startsWith("+")).map(_.drop(1)).toSet
          val removedFields = modifiedFields.filter(_.startsWith("-")).map(_.drop(1)).toSet

          if (addedFields.size+removedFields.size != modifiedFields.size)
            throw new Exception("Malformed line: "+signature+". One or more modifier fields does not start with '-' or '+'")

          (className, addedFields, removedFields)
        }
      }
    }

    private def parseDataType(line: String): JsObject = {
      val parameters = getParameters()
      val fieldNames = parameters.keys.toList.toSet

      val (name, signature) = if (line.endsWith(":")) {

        val signature = key.dropRight(1).trim
        val splitPos = line.indexOf('(')

        if (splitPos== -1)
          (signature, signature+"()")
        else
          (line.take(splitPos).trim, signature)

      } else {

        val splitPos = line.indexOf(':')
        val name = line.take(splitPos).trim
        val signature = line.drop(splitPos+1).trim

        (name, signature)
      }

      if (signature != "!") {
        val (className, addedFields, removedFields) = parseScalaTypeSignature(signature)
        validateDataTypeFields(className, name, fieldNames, addedFields, removedFields)
      }

      Json.obj(
        name -> parameters
      )
    }

    def getApidoc(): JsObject = {
      if (key.startsWith("GET ") || key.startsWith("POST ") || key.startsWith("PUT ") || key.startsWith("DELETE ") || key.startsWith("PATCH ")) {

        if (!elements.isEmpty)
          throw new Exception(s"""Elements for "$key" are not empty: $elements""")

        val pos = key.indexOf(' ')
        val method = key.substring(0,pos).trim
        val uri = key.drop(pos).trim
        val uriParms = findUriParm(uri)

        Json.obj(
          "method" -> method,
          "uri"    -> uri,
          "uriParms" -> uriParms
        )
      }

      else if (key=="DESCRIPTION")
        Json.obj(
          "shortDescription" -> elements.head,
          "longDescription"  -> (if (elements.length==1) "" else elements.tail.mkString("<br>"))
        )

      else if (key=="PARAMETERS")
        Json.obj(
          "parameters" -> getParameters()
        )

      else if (key=="ERRORS")
        Json.obj(
          "errors" -> JsArray(
            elements.map(element => {
              val code = element.substring(0,4).trim.toInt
              val description = element.drop(4).trim
              Json.obj(
                "code" -> code,
                "message" -> description
              )})
          ))

      else if (key=="RESULT") {
        if (elements.length!=1)
          throw new Exception(s"Malformed RESULT elements (more or less than 1): $elements.")

        val splitted = elements(0).trim.split("<-").map(_.trim)
        val type_ = splitted(0)
        val comment = if (splitted.length==1) "" else splitted(1)

        Json.obj(
          "result" -> Json.obj(
            "type" -> type_,
            "comment" -> comment
          )
        )
      }

      else if (key.contains(":"))
        Json.obj(
          "datatype" -> parseDataType(key)
        )

      else
        throw new Exception(s"""Unknown key: "$key"""")
    }
  }

  private def parseRaw(
    lines: List[String],
    current: Raw,
    result: List[Raw],
    mainIndentLength: Int
  ): List[Raw] =

    if (lines.isEmpty)
      current::result

    else {
      val indentLength = getIndentLength(lines.head)
      var line = lines.head.trim
      while(line(0)=='_')
        line = "&nbsp;" + line.drop(1)

      if (indentLength < mainIndentLength)
        throw new Exception(s"""Bad indentation for line "$line"""")

      else if (indentLength > mainIndentLength)
        parseRaw(lines.tail, current.plus(line), result, mainIndentLength)

      else
        parseRaw(lines.tail, Raw(line, List()), current::result, mainIndentLength)
    }

  private def parseRaw(apidoc: String): List[Raw] = {
    val lines = apidoc.split("\n").filter(line => line.trim.length>0).toList // All non-empty lines.
    val indentLength = getIndentLength(lines.head)
    val line = lines.head.trim

    parseRaw(lines.tail, Raw(line, List()), List(), indentLength)
  }

  def getRaw(apidoc: String): JsObject = {
    val raws = parseRaw(apidoc)
    JsObject(raws.map(raw => raw.key -> JsArray(raw.elements.map(JsString))))
  }


  private def validateJson(json: JsObject) = {
    val uriParms = (json \ "uriParms").as[List[String]]
    val pathParms = if (!json.keys.contains("parameters"))
                      Set()
                    else
                      (json \ "parameters").as[JsObject].value.filter(
                         kv => ((kv._2)\"paramType")==JsString("path")
                      ).keys

    if (uriParms.size != pathParms.size)
      throw new MismatchPathParametersException(s"""Mismatch between the number of parameters in the uri, and the number of path parameters.\nuriParms: $uriParms\npathParms:$pathParms\njson: $json).""")

    pathParms.foreach(pathParm =>
      if (!uriParms.contains(pathParm))
        throw new MismatchPathParametersException(s"""The path parameter "${pathParm}" is not defined in the path.""")
    )
  }


  def getJson(apidoc: String): JsObject = {
    val raws = parseRaw(apidoc)
    var ret = Json.obj()
    raws.reverse.map(_.getApidoc).foreach(apidoc =>
      if ( ! apidoc.keys.contains("datatype"))
        ret = ret ++ apidoc
    )
    validateJson(ret)
    ret
  }

  def getDataTypes(apidoc: String): JsObject = {
    val raws = parseRaw(apidoc)
    var ret = Json.obj()
    raws.reverse.map(_.getApidoc).foreach(apidoc =>
      if (apidoc.keys.contains("datatype"))
        ret = ret ++ (apidoc \ "datatype").asInstanceOf[JsObject]
    )
    ret
  }
}
