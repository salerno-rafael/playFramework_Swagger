package component

import dao.IncentiveDao
import service.IncentiveService

trait IncentiveComponent {
  
  lazy val incentiveDao: IncentiveDao = IncentiveLocator.incentiveDao
  lazy val incetiveService: IncentiveService = IncentiveLocator.incetiveService
  
  object IncentiveLocator{
    lazy val incentiveDao = new IncentiveDao ()
    lazy val incetiveService = new IncentiveService(incentiveDao)
  }
}