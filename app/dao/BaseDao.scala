package dao

import play.api.Play._
import scalikejdbc._

trait BaseDao {

  Class.forName(current.configuration.getString("db.default.driver").getOrElse(""))
  ConnectionPool.singleton(
    current.configuration.getString("db.default.url").getOrElse(""),
    current.configuration.getString("db.default.user").getOrElse(""),
    current.configuration.getString("db.default.password").getOrElse(""))

  val settings = ConnectionPoolSettings(
    initialSize = 5,
    maxSize = 20,
    connectionTimeoutMillis = 3000L,
    validationQuery = "select 1 from dual")

    implicit val session = AutoSession
}