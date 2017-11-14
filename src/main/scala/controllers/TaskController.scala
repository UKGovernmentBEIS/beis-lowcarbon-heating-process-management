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

import java.text.DecimalFormat
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
import org.apache.commons.lang3.StringUtils

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

        val assessorgroup = Config.config.bpm.assessorgroup
        val grp = List(assessorgroup)

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
          case "confirmEmailSent" =>
            Future(Ok(views.html.confirmEmailSent(tsk, appFrontEndUrl)))
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
    val sortstr = request.queryString.getOrElse("sort", List()).headOption.getOrElse("")

    println("=========111======="+ sortstr)

   val userId = request.session.get("username").getOrElse("Unauthorised User")
    val ts = localtasks.showTasks(UserId(userId))

    ts.flatMap{
      case ts =>
        sortstr match {
          case "task-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.key))))
          case "task-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.key).reverse )))
          case "app-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.appRef))))
          case "app-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.appRef).reverse )))
          case "technology-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.technology))))
          case "technology-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.technology).reverse )))
          case "aws-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averageweightedscore))))
          case "aws-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averageweightedscore).reverse )))
          case "atbs-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averagetiebreakscore))))
          case "atbs-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averagetiebreakscore).reverse )))
          case "status-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.status))))
          case "status-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.status).reverse )))
          case _ =>
            Future (Ok(views.html.tasks(ts)))
        }
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

    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val status = getValueFromRequest("eligibilitystatus", mp )
    val technology = getValueFromRequest("technology", mp )
    val comment = getValueFromRequest("comment", mp )
    val processInstanceId = getValueFromRequest("processInstanceId", mp )

    localtasks.submitEligibility(id, UserId(userId), status, comment, technology, processInstanceId).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent

    }
  }

  def submitAssignAssessors (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val comment = getValueFromRequest("comment", mp )
    val processInstanceId = getValueFromRequest("processInstanceId", mp )
    val assignassessor1 = getValueFromRequest("assignassessor1", mp )
    val assignassessor2 = getValueFromRequest("assignassessor2", mp )
    val assignassessor3 = getValueFromRequest("assignassessor3", mp )

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
    val projectdescweight =            getValueFromRequest("projectdescweight", mp ).toInt

    val performanceenhancement =       getValueFromRequest("performanceenhancementoption", mp ).toInt
    val performancemanagement =        getValueFromRequest("performancemanagementoption", mp ).toInt
    val performanceintegration =       getValueFromRequest("performanceintegrationoption", mp ).toInt
    val performanceenhancementweight = getValueFromRequest("performanceenhancementweight", mp ).toInt
    val performancemanagementweight =  getValueFromRequest("performancemanagementweight", mp ).toInt
    val performanceintegrationweight = getValueFromRequest("performanceintegrationweight", mp ).toInt
    val performancecomment =           getValueFromRequest("performancecomment", mp )

    val marketpotential =              getValueFromRequest("marketpotentialoption", mp ).toInt
    val marketpotentialcomment =       getValueFromRequest("marketpotentialcomment", mp )
    val marketpotentialweight =        getValueFromRequest("marketpotentialweight", mp ).toInt

    val projectdelivery =              getValueFromRequest("projectdeliveryoption", mp ).toInt
    val projectdeliverycomment =       getValueFromRequest("projectdeliverycomment", mp )
    val projectdeliveryweight =        getValueFromRequest("projectdeliveryweight", mp ).toInt

    val projectfinancing =             getValueFromRequest("projectfinancingoption", mp ).toInt
    val projectfinancingcomment =      getValueFromRequest("projectfinancingcomment", mp )
    val projectfinancingweight =       getValueFromRequest("projectfinancingweight", mp ).toInt

    val widerobjective =               getValueFromRequest("widerobjectiveoption", mp ).toInt
    val widerobjectivecomment =        getValueFromRequest("widerobjectivecomment", mp )
    val widerobjectiveweight =         getValueFromRequest("widerobjectiveweight", mp ).toInt

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
    val performanceenhancementScore = performanceenhancement * performanceenhancementweight / 100.0
    val performancemanagementScore = performancemanagement * performancemanagementweight / 100.0
    val performanceintegrationScore = performanceintegration * performanceintegrationweight / 100.0
    val marketpotentialScore = marketpotential * marketpotentialweight / 100.0
    val projectdeliveryScore = projectdelivery * projectdeliveryweight / 100.0
    val projectfinancingScore = projectfinancing * projectfinancingweight / 100.0
    // Wider objective - Tie Breaker only - Dont add it now
    val widerobjectiveScore = widerobjective * widerobjectiveweight / 100.0


    val weightedScore = performanceenhancementScore + performancemanagementScore + performanceintegrationScore + marketpotentialScore
                        + projectdeliveryScore + projectfinancingScore

    val tiebreakScore = weightedScore + widerobjectiveScore


    /*println("=== score " + "" + "==  " + performanceenhancement)
    println("=== score " + "" + "==  " + performancemanagement)
    println("=== score " + "" + "==  " + performanceintegration)
    println("=== score " + "" + "==  " + marketpotential)
    println("=== score " + "" + "==  " + projectdelivery)
    println("=== score " + "" + "==  " + projectfinancing)
    println("=== score " + "" + "==  " + widerobjective)

    println("=== score weight" + "" + "==  " + performanceenhancementweight)
    println("=== score weight" + "" + "==  " + performancemanagementweight)
    println("=== score weight" + "" + "==  " + performanceintegrationweight)
    println("=== score weight" + "" + "==  " + marketpotentialweight)
    println("=== score weight" + "" + "==  " + projectdeliveryweight)
    println("=== score weight" + "" + "==  " + projectfinancingweight)
    println("=== score weight" + "" + "==  " + widerobjectiveweight)



    println("=== score Score" + "" + "==  " + performanceenhancementScore)
    println("=== score Score" + "" + "==  " + performancemanagementScore)
    println("=== score Score" + "" + "==  " + performanceintegrationScore)
    println("=== score Score" + "" + "==  " + marketpotentialScore)
    println("=== score Score" + "" + "==  " + projectdeliveryScore)
    println("=== score Score" + "" + "==  " + projectfinancingScore)
    println("=== score Score" + "" + "==  " + widerobjectiveScore)
    println("=== weightedScore " + "" + "==  " + weightedScore)
    println("=== tiebreakScore " + "" + "==  " + tiebreakScore)*/


    val score = Score(
      projectdesc, projectdesccomment, projectdescweight.toInt,
      performanceenhancement.toInt, performanceenhancementweight.toInt,
      performancemanagement.toInt, performancemanagementweight.toInt,
      performanceintegration.toInt, performancecomment, performanceintegrationweight.toInt,
      marketpotential.toInt, marketpotentialcomment, marketpotentialweight.toInt,
      projectdelivery.toInt, projectdeliverycomment, projectdeliveryweight.toInt,
      projectfinancing.toInt, projectfinancingcomment, projectfinancingweight.toInt,
      widerobjective.toInt, widerobjectivecomment, widerobjectiveweight.toInt,
      overallcomment, BigDecimal(weightedScore).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble,
      BigDecimal(tiebreakScore).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    )

    printAll(score) //Todo:- Delete this

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

    localtasks.submitMakePanelDecision(id, UserId(userId), status, comment, processInstanceId).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent

    }
  }


  def submitModerateScore (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val averagemoderatescore = request.body.asFormUrlEncoded.getOrElse(Map()).get("averagemoderatescore").headOption.map( _.head).getOrElse("")
    val comment = request.body.asFormUrlEncoded.getOrElse(Map()).get("comment").headOption.map( _.head).getOrElse("")
    val processInstanceId = request.body.asFormUrlEncoded.getOrElse(Map()).get("processInstanceId").headOption.map( _.head).getOrElse("")

    localtasks.submitModerateScore(id, UserId(userId), averagemoderatescore, comment, processInstanceId).map {
      case Some(t) => {
        val ts = localtasks.showTasks(UserId(userId))
        Redirect(controllers.routes.TaskController.tasks())
      }
      case _ => NoContent

    }
  }


  def assessorReport (id : LocalTaskId, assessorid: Int) = Action.async { implicit request =>
    val t = localtasks.showTask(id)
    t.flatMap{
      case Some(tsk) => {

        val appFrontEndUrl = Config.config.business.appFrontEndUrl
        Future(Ok(views.html.assessorReport(tsk, appFrontEndUrl, assessorid)))
      }
      case None => Future.successful(NotFound)
    }
  }

  def submitConfirmEmailSent (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val emailsent = getValueFromRequest("emailsent", mp )
    val comment = getValueFromRequest("comment", mp )
    val processInstanceId = getValueFromRequest("processInstanceId", mp )

    val ems = emailsent match {
      case s if StringUtils.isEmpty(s)  => "No email sent"
      case s if !StringUtils.isEmpty(s)  => "Email sent"
      case _ => "No email sent"
    }

    localtasks.submitConfirmEmailSent(id, UserId(userId), ems, comment, processInstanceId).map {
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

  private def printAll(score: Score) {
    println("--------------------*********" + ":" + "***********-----------------------------")
    println("=== In Task Controller == projectdesc" + ":" + "       :-" + score.projectdesc)
    println("=== In Task Controller == projectdesccomment" + ":" + ":-" + score.projectdesccomment)
    println("=== In Task Controller == projectdescweight" + ":" + " :-" + score.projectdescweight)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performanceenhancement)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performanceenhancementweight)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performancemanagement)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performancemanagementweight)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performanceintegration)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performanceintegrationweight)
    println("=== In Task Controller == performance" + ":" + "       :-" + score.performancecomment)
    println("=== In Task Controller == marketpotential" + ":" + "   :-" + score.marketpotential)
    println("=== In Task Controller == marketpotential" + ":" + "   :-" + score.marketpotentialcomment)
    println("=== In Task Controller == marketpotential" + ":" + "   :-" + score.marketpotentialweight)
    println("=== In Task Controller == projectdelivery" + ":" + "   :-" + score.projectdelivery)
    println("=== In Task Controller == projectdelivery" + ":" + "   :-" + score.projectdeliverycomment)
    println("=== In Task Controller == projectdelivery" + ":" + "   :-" + score.projectdeliveryweight)
    println("=== In Task Controller == projectfinancing" + ":" + "  :-" + score.projectfinancing)
    println("=== In Task Controller == projectfinancing" + ":" + "  :-" + score.projectfinancingcomment)
    println("=== In Task Controller == projectfinancing" + ":" + "  :-" + score.projectfinancingweight)
    println("=== In Task Controller == widerobjective" + ":" + "    :-" + score.widerobjective)
    println("=== In Task Controller == widerobjective" + ":" + "    :-" + score.widerobjectivecomment)
    println("=== In Task Controller == widerobjective" + ":" + "    :-" + score.widerobjectiveweight)
    println("=== In Task Controller == overallcomment" + ":" + "    :-" + score.overallcomment)
    println("=== In Task Controller == weightedscore" + ":" + "    :-" + score.weightedscore)
    println("=== In Task Controller == tiebreakscore" + ":" + "    :-" + score.tiebreakscore)
  }
}

case class FieldError(path: String, err: String)


