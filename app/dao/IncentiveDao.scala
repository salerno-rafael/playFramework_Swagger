package dao

import play.api.Play._
import scalikejdbc._

class IncentiveDao extends BaseDao {

  case class IncentiveEntity(
    comarOrder: String,
    orderNumber: String,
    invoiceNumber: String,
    serialNumber: String,
    bmc: String,
    modelDescription: String,
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
    quotationNumber: String,
    sapDealerCode:String,
    dealerAccount:String)

  object IncentiveEntity extends SQLSyntaxSupport[IncentiveEntity] {
    override val tableName = "IMPORT_ZVDISCOUNT"
    def apply(rs: WrappedResultSet) = new IncentiveEntity(
      rs.string("ordercomar"),
      rs.string("ordernumber"),
      rs.string("nfenumber"),
      rs.string("serialnumber"),
      rs.string("material"),
      rs.string("description"),
      rs.string("ordercreationdate"),
      rs.string("preSoldate"),
      rs.string("retailSoldDate"),
      rs.string("invoicingdate"),
      rs.string("totalcost"),
      rs.string("documentdate"),
      rs.string("discountcode"),
      rs.string("description_code"),
      rs.string("discountperc"),
      rs.string("netprice"),
      rs.string("quotationnumber"),
      rs.string("sapdealercode"),
      rs.string("dealeraccount"))
  }

  def incentives(orderComar: Option[String], quotationNumber: Option[String], chassi: Option[String], sapInvoiceNumber: Option[String]): List[IncentiveEntity] = {
    sql"""
      select est.ordercomar,dis.ordernumber,est.nfenumber,est.serialnumber,dis.material,
      dis.description,dis.ordercreationdate,com.marksolddate as preSoldate,sal.retailsolddate as retailSoldDate,dis.invoicingdate,
      est.totalcost,est.documentdate,dis.discountcode,discount_description.description as description_code,dis.discountperc,dis.netprice,
      dis.quotationnumber,sapdealercode,dealeraccount
      from zvestat_zvdiscount zz 
      inner join import_zvdiscount dis on dis.id = zz.id_zvdiscount
      inner join import_zvestat est on est.id = zz.id_zvestat
      inner join discount_description on ct_code = dis.discountcode
	  left join comar com on com.serialnumber = est.serialnumber
      left join salessystem sal on sal.serialnumber = est.serialnumber
      where 1=1
      ${commarHolder(orderComar)} ${quotationNumberHolder(quotationNumber)} ${chassiHolder(chassi)} ${sapInvoiceNumberHolder(sapInvoiceNumber)}
  """.map(rs => IncentiveEntity(rs)).list.apply()
  }

  private def commarHolder(orderComar: Option[String]): scalikejdbc.interpolation.SQLSyntax =
    orderComar.map(x => sqls" And est.ordercomar = ${x}").getOrElse(sqls"")

  private def quotationNumberHolder(quotationNumber: Option[String]): scalikejdbc.interpolation.SQLSyntax =
    quotationNumber.map(x => sqls" And dis.quotationnumber = ${x}").getOrElse(sqls"")

  private def chassiHolder(chassi: Option[String]): scalikejdbc.interpolation.SQLSyntax =
    chassi.map(x => sqls" And est.serialnumber = ${x}").getOrElse(sqls"")

  private def sapInvoiceNumberHolder(sapInvoiceNumber: Option[String]): scalikejdbc.interpolation.SQLSyntax =
    sapInvoiceNumber.map(x => sqls" And est.nfenumber = ${x}").getOrElse(sqls"")
  
  def addIncentive(quotationNumber: String, bulletinNumber: String, invoiceAmount: String, incetivePercent: String, approverI: String, approverII: String, irsRequisitionNumber: String) {
    println(quotationNumber + " " + bulletinNumber + " " + invoiceAmount + " " + incetivePercent + " " + approverI + " " + approverII + " " + irsRequisitionNumber);
  }
}