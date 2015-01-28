package view.model

import service.Incentive
import play.api.libs.json._

case class IncentiveOutput(
  comarOrder: String,
  sap: SapOutput,
  serialNumber: String,
  bmc: String,
  modelDescription: String,
  dealerAccount: String,
  orderDate: String,
  preSoldDate: String,
  retailSoldDate: String,
  invoiceDate: String,
  invoiceAmount: String,
  documentDate: String,
  quotationNumber: String,
  discounts: List[DiscountOutput])

case class SapOutput(orderNumber: String, invoiceNumber: String, sapDealerCode: String)

case class DiscountOutput(ctCode: String, ctDescription: String, percentageAmount: String, netPrice: String, currencyType: String)

object ViewModel {

  def buildIncentive(incentives: List[Incentive]): List[IncentiveOutput] = {
     incentives.groupBy(f => f.comarOrder).mapValues { f =>
      IncentiveOutput(f.head.comarOrder, SapOutput(f.head.sap.orderNumber, f.head.sap.invoiceNumber, f.head.sap.sapDealerCode),
        f.head.serialNumber, f.head.bmc, f.head.modelDescription, f.head.dealerAccount, f.head.orderDate,
        f.head.preSoldDate, f.head.retailSoldDate, f.head.invoiceDate, f.head.invoiceDate, f.head.documentDate,
        f.head.quotationNumber,
        f.map(x => DiscountOutput(x.ctCode, x.ctDescription, x.percentageAmount, x.netPrice, "BRL")))
    }.map(_._2).toList
  }

 
  implicit val discountWrites = new Writes[DiscountOutput] {
    def writes(discount: DiscountOutput) = Json.obj(
        "ctCode" -> discount.ctCode,
        "ctDescription" -> discount.ctDescription ,
        "percentageAmount" -> discount.percentageAmount,
        "netPrice" -> discount.netPrice,
        "currencyType" -> discount.currencyType )
  }

  implicit val sapWrites = new Writes[SapOutput] {
    def writes(sap: SapOutput) = Json.obj(
         "orderNumber" -> sap.orderNumber,
        "invoiceNumber" -> sap.invoiceNumber,
        "sapDealerCode" -> sap.sapDealerCode)
  }

  implicit val incentiveWrites = new Writes[IncentiveOutput] {
    def writes(incentive: IncentiveOutput) = Json.obj(
        "comarOrder" -> incentive.comarOrder,
        "sap" -> incentive.sap,
        "serialNumber" -> incentive.serialNumber,
        "Bmc" -> incentive.bmc,
        "modelDescription" -> incentive.modelDescription,
        "dealerAccount" -> incentive.dealerAccount,
        "orderDate" -> incentive.orderDate,
        "preSoldDate" -> incentive.preSoldDate,
        "retailSoldDate" -> incentive.retailSoldDate,
        "invoiceDate" -> incentive.invoiceDate,
        "invoiceAmount" -> incentive.invoiceAmount,
        "documentDate" -> incentive.documentDate,
        "quotationNumber" -> incentive.quotationNumber,
        "discounts"-> incentive.discounts)
  }
  
  def incentiveToJson(incentives:List[IncentiveOutput])={
    Json.toJson(incentives)
  }
}