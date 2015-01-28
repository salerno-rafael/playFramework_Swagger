package controllers

import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current
import swagger.helper.apidoc.{ ApiDoc, ApiDocUtil, SwaggerUtil }
import service.IncentiveService
import scala.concurrent.Future
import dao.IncentiveDao
import component.IncentiveComponent
import view.model._

case class IncentiveInput(orderComar: Option[String], quotationNumber: Option[String], chassi: Option[String], sapInvoiceNumber: Option[String])

object IncentiveController extends Controller with IncentiveComponent {

  private val AccessControlAllowOrigin = ("Access-Control-Allow-Origin", "*")

  private def buildInput(request: play.api.mvc.Request[play.api.mvc.AnyContent]) =
    IncentiveInput(
      request.queryString.get("orderComar").flatMap(_.headOption),
      request.queryString.get("quotationNumber").flatMap(_.headOption),
      request.queryString.get("serialNumber").flatMap(_.headOption),
      request.queryString.get("sapInvoiceNumber").flatMap(_.headOption))

  @ApiDoc(doc = """
    GET  /incentive/api/v1/incentive

    DESCRIPTION
      Get incentive
      This service provide by filter incentive on RSI database

    PARAMETERS
      orderComar: String (query, optional) <- default = String Empty
      quotationNumber: String (query, optional) <- default = String Empty
      serialNumber: String (query, optional) <- default = String Empty
      sapInvoiceNumber: String (query, optional) <- default = String Empty
      format: String (query, optional) <- values = xml / json

    RESULT
      IncentiveOutput

    ERRORS
      404 incentive not found
      400 Syntax Error

    IncentiveOutput: view.model.IncentiveOutput
      comarOrder: String
      sap: SapOutput
      serialNumber: String
      bmc: String
      modelDescription: String
      dealerAccount: String
      orderDate: String
      preSoldDate: String
      retailSoldDate: String
      invoiceDate: String
      invoiceAmount: String
      documentDate: String
      quotationNumber: String
      discounts: List[view.model.DiscountOutput]
    
    DiscountOutput: view.model.DiscountOutput
      ctCode: String
      ctDescription: String
      percentageAmount: String
      netPrice: String
      currencyType: String
    
    SapOutput: view.model.SapOutput
      orderNumber: String
      invoiceNumber: String
      sapDealerCode: String 
    
  """)
  def getIncenitves = Action { request =>
    request.queryString.get("format").flatMap(_.map(_.toLowerCase()).headOption) match {
      case Some("xml") => Ok(getXml(buildInput(request))).withHeaders(AccessControlAllowOrigin)
      case Some("json") => Ok(getJson(buildInput(request))).withHeaders(AccessControlAllowOrigin)
      case _ => Ok(getXml(buildInput(request))).withHeaders(AccessControlAllowOrigin)
    }
  }

  private def getXml(input: IncentiveInput) = {
    views.xml.output.IncentiveResponse(
      ViewModel.buildIncentive(
        incetiveService.listIncentive(
          input.orderComar,
          input.quotationNumber,
          input.chassi,
          input.sapInvoiceNumber)).map(inc => views.xml.output.IncentiveItem(inc)))
  }

  private def getJson(input: IncentiveInput) = {
    ViewModel.incentiveToJson(
      ViewModel.buildIncentive(
        incetiveService.listIncentive(
          input.orderComar,
          input.quotationNumber,
          input.chassi,
          input.sapInvoiceNumber)))
  }

  @ApiDoc(doc = """
    POST  /incentive/api/v1/add/{quotationNumber}/{bulletinNumber}/{invoiceAmount}/{incetivePercent}/{approverI}/{approverII}/{irsRequisitionNumber}

    DESCRIPTION
      Add incentive
      Results are Http Status Code

    PARAMETERS
      quotationNumber: String <- Parameter comment
      bulletinNumber: String <- Parameter comment
      invoiceAmount: String <- Parameter comment
      incetivePercent: String <- Parameter comment
      approverI: String <- Parameter comment
      approverII: String <- Parameter comment
      irsRequisitionNumber: String <- Parameter comment

    ERRORS
      400 Syntax Error
    
  """)
  def addIncentive(quotationNumber: String, bulletinNumber: String, invoiceAmount: String, incetivePercent: String,
    approverI: String, approverII: String, irsRequisitionNumber: String) = Action { implicit request =>
    incetiveService.addIncentive(quotationNumber, bulletinNumber, invoiceAmount, incetivePercent, approverI, approverII, irsRequisitionNumber)
    Ok("INSERIDO COM SUCESSO.").withHeaders(AccessControlAllowOrigin)
  }
}
