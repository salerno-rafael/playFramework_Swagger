package service

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import dao.IncentiveDao
import view.model.ViewModel

case class Incentive(
  comarOrder: String,
  sap: Sap,
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
  ctCode: String,
  ctDescription: String,
  percentageAmount: String,
  netPrice: String,
  quotationNumber: String)

case class Sap(orderNumber: String, invoiceNumber: String, sapDealerCode: String)

class IncentiveService(incetiveDao: IncentiveDao) {

  def listIncentive(orderComar: Option[String], quotationNumber: Option[String], chassi: Option[String], sapInvoiceNumber: Option[String]): List[Incentive] = {
    incetiveDao.incentives(orderComar, quotationNumber, chassi, sapInvoiceNumber).map { inc =>
      Incentive(
        inc.comarOrder,
        Sap(inc.orderNumber, inc.invoiceNumber, inc.sapDealerCode),
        inc.serialNumber,
        inc.bmc,
        inc.modelDescription,
        inc.dealerAccount,
        inc.orderDate,
        inc.preSoldDate,
        inc.retailSoldDate,
        inc.invoiceDate,
        inc.invoiceAmount,
        inc.documentDate,
        inc.ctCode,
        inc.ctDescription,
        inc.percentageAmount,
        inc.netPrice,
        inc.quotationNumber)
    }
  }

  def addIncentive(quotationNumber: String, bulletinNumber: String, invoiceAmount: String, incetivePercent: String, approverI: String, approverII: String, irsRequisitionNumber: String) {
    incetiveDao.addIncentive(quotationNumber, bulletinNumber, invoiceAmount, incetivePercent, approverI, approverII, irsRequisitionNumber)
  }

}  
 