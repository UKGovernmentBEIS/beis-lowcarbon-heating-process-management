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

import javax.inject.Inject

import cats.data.ValidatedNel
import models.{LocalTask, LocalTaskId, Score, UserId}
import play.api.libs.json.JsValue
import play.api.mvc.Results.{NotFound, Redirect}
import play.api.mvc.{Action, Controller}
import services.BEISTaskOps
import config.Config
import play.api.i18n.Messages

import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.{universe => ru}
import cats.data.ValidatedNel
import cats.syntax.cartesian._
import cats.syntax.validated._

import scala.util.{Failure, Success, Try}

class TaskController @Inject()(localtasks: BEISTaskOps )(implicit ec: ExecutionContext) extends Controller {

  def startPage = Action {
    Ok(views.html.startPage())
  }

  def task (id : LocalTaskId, appId : Long, oppId : Long) = Action.async {
    val t = localtasks.showTask(id)
    t.flatMap{
      case Some(tsk) => {

        val appFrontEndUrl = Config.config.business.appFrontEndUrl
        val submitStatusMap = Map("eligible" -> "Eligible", "noteligible" -> "Not Eligible")
        val yesnoMap = Map("no" -> "No", "yes" -> "Yes")
        val technologyMap = ListMap("technology1" -> "Technology1", "technology2" -> "Technology2", "technology3" -> "Technology3",
                                  "technology4" -> "Technology4", "technology5  " -> "Technology5")
        val decisionMap = Map("approved" -> "Approved", "notapproved" -> "Not Approved")
        val scoreMap = ListMap("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4", "5" -> "5", "6" -> "6", "7" -> "7",
                          "8" -> "8", "9" -> "9", "10" -> "10")
        val grp = List("assessor")

        tsk.key match {
          case "assessEligibility" =>
            Future(Ok(views.html.assessEligibility(tsk, appFrontEndUrl, technologyMap, submitStatusMap)))
          case "assignAssessors" =>
            Future(Ok(views.html.assignAssessors(tsk, appFrontEndUrl, getMembers(grp), List())))
          //case "firstAssessment" | "secondAssessment" | "thirdAssessment"=>
          case "firstAssessment"=>
            Future(Ok(views.html.assessment(tsk, appFrontEndUrl, yesnoMap, scoreMap, "1")))
          case "secondAssessment"=>
            Future(Ok(views.html.assessment(tsk, appFrontEndUrl, yesnoMap, scoreMap, "2")))
          case "thirdAssessment"=>
            Future(Ok(views.html.assessment(tsk, appFrontEndUrl, yesnoMap, scoreMap, "3")))
          case "makePanelDecision" =>
            Future(Ok(views.html.makePanelDecision(tsk, appFrontEndUrl, decisionMap)))
          case "moderateScore" =>
            Future(Ok(views.html.moderateScore(tsk, appFrontEndUrl, decisionMap)))
        }
      }
      case None => Future.successful(NotFound)
    }
  }

  def getMembers(s: List[String]) : Map[String, String]={

    val members = localtasks.getMembers(s).getOrElse(Set())
    members.map(s=> s -> s).toMap
  }

  def getMembers_(s: List[String]) : Map[String, String]={
    val members = localtasks.getMembers(s).getOrElse(Set())
    val membersMap = ListMap( (1 to members.size).zip(members): _*)
    membersMap.map(q => (q._1.toString, q._2)).toMap
  }

  def tasks = Action.async  {   implicit request =>
   val userId = request.session.get("username").getOrElse("Unauthorised User")
    val ts = localtasks.showTasks(UserId(userId))

    ts.flatMap{
      case ts =>
        Future(Ok(views.html.tasks(ts)))
      case Seq() => Future.successful(NotFound)
    }
  }

  def submit (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val status = request.body.asFormUrlEncoded.getOrElse(Map()).get("approvestatus").headOption.map( _.head).getOrElse("")
    val comment = request.body.asFormUrlEncoded.getOrElse(Map()).get("comment").headOption.map( _.head).getOrElse("")
    val processInstanceId = request.body.asFormUrlEncoded.getOrElse(Map()).get("processInstanceId").headOption.map( _.head).getOrElse("")

    localtasks.submitProcess(id, UserId(userId), status, comment, processInstanceId).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent

    }
  }

  def submitAssessEligibility (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val status = request.body.asFormUrlEncoded.getOrElse(Map()).get("approvestatus").headOption.map( _.head).getOrElse("")
    val comment = request.body.asFormUrlEncoded.getOrElse(Map()).get("comment").headOption.map( _.head).getOrElse("")
    val processInstanceId = request.body.asFormUrlEncoded.getOrElse(Map()).get("processInstanceId").headOption.map( _.head).getOrElse("")

    localtasks.submitEligibility(id, UserId(userId), status, comment, processInstanceId).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent

    }
  }

  def submitAssignAssessors (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val comment = request.body.asFormUrlEncoded.getOrElse(Map()).get("comment").headOption.map( _.head).getOrElse("")
    val processInstanceId = request.body.asFormUrlEncoded.getOrElse(Map()).get("processInstanceId").headOption.map( _.head).getOrElse("")

    val assignassessor1 = request.body.asFormUrlEncoded.getOrElse(Map()).get("assignassessor1").headOption.map( _.head).getOrElse("")
    val assignassessor2 = request.body.asFormUrlEncoded.getOrElse(Map()).get("assignassessor2").headOption.map( _.head).getOrElse("")
    val assignassessor3 = request.body.asFormUrlEncoded.getOrElse(Map()).get("assignassessor3").headOption.map( _.head).getOrElse("")

    val appFrontEndUrl = Config.config.business.appFrontEndUrl

    val errors = duplicateSelectionCheck(assignassessor1, assignassessor2, assignassessor3).fold(_.toList, _ => List())
    errors.isEmpty match{
      case true =>
        localtasks.submitAssignAssessors (id, UserId (userId), assignassessor1, assignassessor2, assignassessor3, comment,
          processInstanceId).map {
            case Some (t) => {
                val ts = localtasks.showTasks (UserId (userId) )
                Redirect (controllers.routes.TaskController.tasks () )
            }
            case _ => NoContent

        }
      case false =>
          val t = localtasks.showTask(id)
          val grp = List("assessor")

          t.flatMap {
              case Some(lt) =>
                Future.successful(Ok(views.html.assignAssessors(lt, appFrontEndUrl, getMembers(grp), errors)))
              case None =>
                Future.successful(NotFound)
          }
        }
    }

  def submitAssessment (id : LocalTaskId, key: String) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val projectdesc =                  getValueFromRequest("projectdescriptionoption", mp )
    val projectdesccomment =           getValueFromRequest("projectdescriptioncomment", mp )
    val projectdescweight =            getValueFromRequest("projectdescweight", mp )

    val performanceenhancement =       getValueFromRequest("performanceenhancementoption", mp )
    val performancemanagement =        getValueFromRequest("performancemanagementoption", mp )
    val performanceintegration =       getValueFromRequest("performanceintegrationoption", mp )
    val performanceenhancementweight = getValueFromRequest("performanceenhancementweight", mp )
    val performancemanagementweight =  getValueFromRequest("performancemanagementweight", mp )
    val performanceintegrationweight = getValueFromRequest("performanceintegrationweight", mp )
    val performancecomment =           getValueFromRequest("performancecomment", mp )

    val marketpotential =              getValueFromRequest("marketpotentialoption", mp )
    val marketpotentialcomment =       getValueFromRequest("marketpotentialcomment", mp )
    val marketpotentialweight =        getValueFromRequest("marketpotentialweight", mp )

    val projectdelivery =              getValueFromRequest("projectdeliveryoption", mp )
    val projectdeliverycomment =       getValueFromRequest("projectdeliverycomment", mp )
    val projectdeliveryweight =        getValueFromRequest("projectdeliveryweight", mp )

    val projectfinancing =             getValueFromRequest("projectfinancingoption", mp )
    val projectfinancingcomment =      getValueFromRequest("projectfinancingcomment", mp )
    val projectfinancingweight =       getValueFromRequest("projectfinancingweight", mp )

    val widerobj =                     getValueFromRequest("widerobjectiveoption", mp )
    val widerobjcomment =              getValueFromRequest("widerobjectivecomment", mp )
    val widerobjweight =               getValueFromRequest("widerobjectiveweight", mp )

    val overallcomment =               getValueFromRequest("overallcomment", mp )

    val save_button_action =           getValueFromRequest("save", mp )
    val complete_button_action =       getValueFromRequest("complete", mp )

    val button_action:String = if(!getValueFromRequest("save", mp ).equals("")) "save"
                              else if(!getValueFromRequest("complete", mp ).equals("")) "complete" else "save"

    val asmtKey = key match {
      case "firstAssessment" => 1
      case "secondAssessment" => 2
      case "thirdAssessment" => 3
    }

    val score = Score(
      projectdesc, projectdesccomment, projectdescweight.toInt,
      performanceenhancement.toInt, performanceenhancementweight.toInt,
      performancemanagement.toInt, performancemanagementweight.toInt,
      performanceintegration.toInt, performancecomment, performanceintegrationweight.toInt,
      marketpotential.toInt, marketpotentialcomment, marketpotentialweight.toInt,
      projectdelivery.toInt, projectdeliverycomment, projectdeliveryweight.toInt,
      projectfinancing.toInt, projectfinancingcomment, projectfinancingweight.toInt,
      widerobj.toInt, widerobjcomment, widerobjweight.toInt,
      overallcomment
    )

    val processInstanceId = request.body.asFormUrlEncoded.getOrElse(Map()).get("processInstanceId").headOption.map( _.head).getOrElse("")

      localtasks.submitAssessment(id, UserId(userId), asmtKey, score, processInstanceId, button_action).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent
    }
  }

  def getValueFromRequest(key: String, keyValueMap: Map[String, Seq[String]]): String =

    keyValueMap.get(key).headOption.map(_.head).getOrElse("").toString


  def submitMakePanelDecision (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val status = request.body.asFormUrlEncoded.getOrElse(Map()).get("approvestatus").headOption.map( _.head).getOrElse("")
    val comment = request.body.asFormUrlEncoded.getOrElse(Map()).get("comment").headOption.map( _.head).getOrElse("")
    val processInstanceId = request.body.asFormUrlEncoded.getOrElse(Map()).get("processInstanceId").headOption.map( _.head).getOrElse("")

    localtasks.submitEligibility(id, UserId(userId), status, comment, processInstanceId).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent

    }
  }

  def duplicateSelectionCheck(s1: String, s2: String, s3: String): ValidatedNel[FieldError, String] = {

    !s1.equals(s2) && !s1.equals(s3) && !s2.equals(s3) match {
      case false =>  FieldError("assignassessor1", "Please select different Assessors in each selection").invalidNel
      case true => "".validNel
    }
  }
}

case class FieldError(path: String, err: String)


