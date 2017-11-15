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

import org.activiti.engine.impl.persistence.entity.ProcessDefinitionEntity
import org.activiti.engine.repository.ProcessDefinition
import org.activiti.engine.task.Task
import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller, MultipartFormData, Result}
import play.api.mvc.Security
import org.activiti.engine.{ProcessEngine, ProcessEngines}
import scala.concurrent.{ExecutionContext, Future}

/********************************************************************************
  This file is for temporary Login till any Security component is deployed.
  This file also for Activity samples.
  Please donot use this login file. i.e dont use http://localhost:9000/login
  Use only http://localhost:9000
 *********************************************************************************/

class UserController /* @Inject()(pe: ProcessEngine) */ extends Controller {

  implicit val postWrites = Json.writes[LoginForm]

  val loginform:Form[LoginForm] = Form(
    mapping(
      "name" -> text,
      "password" -> text
    ) (LoginForm.apply)(LoginForm.unapply) verifying ("Invalid email or password", result => result match {
      case loginForm => check(loginForm.name, loginForm.password)
    })
  )


  def check(username: String, password: String) = {
    (username == "assessor1" && password == "1234") ||
    (username == "assessor2" && password == "1234") ||
    (username == "assessor3" && password == "1234") ||
    (username == "assessor4" && password == "1234") ||
    (username == "policyadmin1" && password == "1234") ||
    (username == "policyadmin2" && password == "1234") ||
    (username == "approver1" && password == "1234") ||
    (username == "approver2" && password == "1234") ||
    (username == "portfoliomanager" && password == "1234") ||
    (username == "admin" && password == "1234")
  }

  def loginForm = Action{
    Ok(views.html.loginForm("", loginform))
  }

  def loginFormSubmit = Action { implicit request =>
println("=========")
    loginform.bindFromRequest.fold(
      errors => {
        Ok(views.html.loginForm("error", loginform))
      },
      user=> {
        implicit val userIdInSession = user.name
        if(user.name.equals("assessor1") || user.name.equals("assessor2") ||
          user.name.equals("assessor3") || user.name.equals("assessor4")  ||
          user.name.equals("policyadmin1") || user.name.equals("policyadmin2")  ||
          user.name.equals("approver1") || user.name.equals("approver2")  ||
          user.name.equals("admin") )
          Redirect(controllers.routes.TaskController.tasks()).withSession(Security.username -> user.name)
        else
          Redirect(controllers.routes.UserController.loginForm()).withSession(Security.username -> user.name)
      }
    )
  }


}

case class LoginForm(name: String, password: String)
