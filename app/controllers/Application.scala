package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Redirect(routes.Assets.at("swagger/index.html")+"?"+controllers.routes.ApiDocController.get)
  }
}