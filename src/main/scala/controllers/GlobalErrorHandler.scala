package controllers

import javax.inject.Singleton

import play.api.http.HttpErrorHandler
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent._
import javax.inject.Inject

/**
  * Created by venkatamutyala on 14/02/2018.
  */


@Singleton
class GlobalErrorHandler @Inject()(msg: MessagesApi) extends HttpErrorHandler {

  def onClientError(request: RequestHeader, statusCode: Int, message: String) = {
    println("=== ERROR: In GlobalErrorHandler ERROR Request is:=== "+request.toString())
    println("=== ERROR: In GlobalErrorHandler ERROR StatusCode and message are:=== "+statusCode + "========"+ message)
    val errMsg = msg("error.BF040")
    Future.successful(Ok(views.html.loginForm(errMsg, None)))
  }

  def onServerError(request: RequestHeader, exception: Throwable) = {
    println("=== ERROR: In GlobalErrorHandler ERROR Request is:=== "+request.toString())
    println("=== ERROR: In GlobalErrorHandler ERROR Exception is:=== "+exception)
    val errMsg = msg("error.BF040")
    Future.successful(Ok(views.html.loginForm(errMsg, None)))
  }
}
