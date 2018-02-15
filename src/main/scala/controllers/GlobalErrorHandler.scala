package controllers

import play.api.http.HttpErrorHandler
import play.api.mvc._
import play.api.mvc.Results._

import scala.concurrent._
import javax.inject.Singleton

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
/**
  * Created by venkatamutyala on 14/02/2018.
  */

@Singleton
class GlobalErrorHandler extends HttpErrorHandler {

  implicit val messages = Messages

  def onClientError(request: RequestHeader, statusCode: Int, message: String) = {

    val errMsg = Messages("error.BF010")
    Future.successful(Ok(views.html.loginForm(errMsg, None)).withNewSession)
  }

  def onServerError(request: RequestHeader, exception: Throwable) = {

    val errMsg = Messages("error.BF010")
    Future.successful(Ok(views.html.loginForm(errMsg, None)).withNewSession)
  }

}
