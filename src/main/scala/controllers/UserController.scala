/*
 * Copyright (C) 2016  Department for Business, Energy and Industrial Strategy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package controllers

import java.util
import javax.inject.Inject

import config.Config
import models.UserId
import org.activiti.engine.impl.persistence.entity.ProcessDefinitionEntity
import org.activiti.engine.repository.ProcessDefinition
import org.activiti.engine.task.Task
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.json.Json
import org.activiti.engine.{ProcessEngine, ProcessEngines}
import org.apache.commons.lang3.StringUtils

import scala.concurrent.{ExecutionContext, Future}
import play.api.data.Forms._
import play.api.i18n.MessagesApi
import services.BEISTaskOps
import validations.PasswordValidator
import validations.FieldError

/********************************************************************************
  This file is for temporary Login till any Security component is deployed.
  This file also for Activity samples.
  Please donot use this login file. i.e dont use http://localhost:9000/login
  Use only http://localhost:9000
 *********************************************************************************/

class UserController @Inject()(localtasks: BEISTaskOps, msg: MessagesApi )(implicit ec: ExecutionContext) extends Controller {

  implicit val postWrites = Json.writes[LoginForm]

  val loginform:Form[LoginForm] = Form(
    mapping(
      "name" -> text.verifying("Please enter a value in 'User name field", {!_.isEmpty}),
      "password" -> text.verifying("Please enter a value in 'Password' field", {!_.isEmpty})
    ) (LoginForm.apply)(LoginForm.unapply) verifying ("Invalid password", result => result match {
      case loginForm => check(loginForm.name, loginForm.password)
    })
  )

  val registrationform:Form[RegistrationForm] = Form(
    mapping(
      "firstname" -> text,
      "lastname" -> text,
      "password" -> text,
      "confirmpassword" -> text,
      "email" -> text
    ) (RegistrationForm.apply)(RegistrationForm.unapply) verifying ("Invalid email or password", result => result match {
      case registrationForm => check(registrationForm.firstname, registrationForm.lastname, registrationForm.password,
        registrationForm.confirmpassword, registrationForm.email)
    })
  )

  val passwordresetform:Form[PasswordResetForm] = Form(
    mapping(
      "password" -> text.verifying("Please enter a value in 'Password' field", {!_.isEmpty}),
      "newpassword"-> text.verifying("Please enter a value in 'New Password' field", {!_.isEmpty}),
      "confirmpassword" -> text.verifying("Please enter a value in 'Confirm Password' field", {!_.isEmpty})
    ) (PasswordResetForm.apply)(PasswordResetForm.unapply) verifying (msg("error.BF001"), result => result match {
      case passwordresetForm =>
        checkPasswordReset(passwordresetForm.newpassword, passwordresetForm.confirmpassword)
      //case _ =>  false
    })
  )

  def passswordCheck(path: String, s: Option[String], displayName: String): List[FieldError] =
    (PasswordValidator(Some(displayName)).validate(path, s)).fold(_.toList, _ => List())

  def checkPasswordReset(feild1: String, feild2: String) = {
    ( feild1.equals(feild2))
  }

  def check(feild1: String, feild2: String) = {
    (StringUtils.isNotBlank(feild1) && StringUtils.isNotBlank(feild2) )
  }

  def check(firstname: String, lastname: String, password: String, confirmpassword: String, email: String) = {
    (StringUtils.isNotBlank(firstname) && StringUtils.isNotBlank(password) )
  }

  def loginForm = Action{
    Ok(views.html.loginForm("", Some(loginform))).withNewSession
  }

  def registrationForm = Action{ implicit request =>
    Ok(views.html.registrationForm(registrationform, List()))
  }

  def passwordresetForm = Action{ implicit request =>
    val userId = request.session.get("username_process").getOrElse("Unauthorised User")
    Ok(views.html.passwordForm(passwordresetform, userId, List()))
  }

  def loginFormSubmit = Action.async { implicit request =>

    loginform.bindFromRequest.fold(
      errors => {
        Future.successful(Ok(views.html.loginForm(errors.errors.head.message, Some(loginform))))
      },
      user=> {
        implicit val userIdInSession = user.name
        val grp = localtasks.submitLogin(user.name, user.password)

        grp.flatMap{
          case Some(g) => {
            val appFrontEndUrl = Config.config.business.appFrontEndUrl
            Future.successful(Redirect(controllers.routes.TaskController.tasks_processes("task-asc")).withSession(
              ("username_process" -> user.name), ("role" -> g), ("sessionTime" -> System.currentTimeMillis.toString)))
          }
          case None => Future.successful(NotFound)
            println("Error in loginFormSubmit - no Group")

            val errMsg = msg("error.BF002")
            Future.successful(Ok(views.html.loginForm(errMsg, Some(loginform))))
        }
       }
    )
  }

  def getValueFromRequest(key: String, keyValueMap: Map[String, Seq[String]]): String =

    keyValueMap.get(key).headOption.map(_.head).getOrElse("").toString


  def updatePassword =  Action.async { implicit request =>

    val userId = request.session.get("username_process").getOrElse("Unauthorised User")

    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val password = getValueFromRequest("password", mp )
    val newpassword = getValueFromRequest("newpassword", mp )
    val confirmpassword = getValueFromRequest("confirmpassword", mp )


    val pswrdErrors = passswordCheck("newpassword", Some(newpassword), "newpassword")

    passwordresetform.bindFromRequest.fold(
      errors => {
        Future.successful(Ok(views.html.passwordForm(passwordresetform, userId, List(FieldError("",errors.errors.head.message)))))
      },
      user=> {
        //implicit val userIdInSession = user.name
        pswrdErrors.isEmpty match {
          case true =>
            val userSaved = localtasks.updatePassword(userId, password, newpassword)

            userSaved.flatMap{
              case 1 => {
                val appFrontEndUrl = Config.config.business.appFrontEndUrl
                Future.successful(Ok(views.html.loginForm("", Some(loginform), Some(true))))
              }
              case 0 => {
                //Future.successful(NotFound)
                val errMsg = msg("error.BF003")
                Future.successful(Ok(views.html.passwordForm(passwordresetform, userId, List(FieldError("", errMsg)))))
              }
              case 2 => {
                //Future.successful(NotFound)
                val errMsg = msg("error.BF005")
                Future.successful(Ok(views.html.passwordForm(passwordresetform, userId, List(FieldError("", errMsg)))))
              }
            }

          case false =>
            Future.successful(Ok(views.html.passwordForm(passwordresetform, userId, pswrdErrors)))
        }

      }
    )
  }

  def registrationSubmit =  Action.async { implicit request =>

    val userId = request.session.get("username_process").getOrElse("Unauthorised User")
    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val firstname = getValueFromRequest("firstname", mp )
    val lastname = getValueFromRequest("lastname", mp )
    val password = getValueFromRequest("password", mp )
    val confirmpassword = getValueFromRequest("confirmpassword", mp )
    val email = getValueFromRequest("email", mp )

    registrationform.bindFromRequest.fold(
      errors => {
        println("Error in registrationSubmit")
        Future.successful(Ok(views.html.loginForm("error", Some(loginform))))
      },
      user=> {
        //implicit val userIdInSession = user.name
        val userSaved = localtasks.saveUser(userId, user.firstname, user.lastname, user.password, user.email)

        userSaved.flatMap{
          case 1 => {
            val appFrontEndUrl = Config.config.business.appFrontEndUrl
            Future.successful(Ok(views.html.loginForm("", Some(loginform))))
          }
          case 0 => Future.successful(NotFound)
            println("Error in registrationSubmit - User Save error")
            val errMsg = msg("error.BF002")
            Future.successful(Ok(views.html.loginForm(errMsg, Some(loginform))))
        }
      }
    )
  }

  def logOut = Action{
    Ok(views.html.loginForm("", Some(loginform))).withNewSession
  }

}

case class LoginForm(name: String, password: String)
case class RegistrationForm(firstname: String, lastname: String, password: String, confirmpassword: String, email: String)
case class PasswordResetForm(password: String, newpassword: String, confirmpassword: String)
