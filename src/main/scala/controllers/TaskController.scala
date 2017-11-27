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
import play.api.mvc.{Action, Controller, Flash}
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
import validations.FieldError
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._


class TaskController @Inject()(localtasks: BEISTaskOps )(implicit ec: ExecutionContext) extends Controller {

  implicit val messages = Messages

  def startPage = Action {
    Ok(views.html.startPage())
  }

  def task (id : LocalTaskId, appId : Long, oppId : Long) = Action.async { implicit request =>
    val t = localtasks.showTask(id)
    val userId = request.session.get("username").getOrElse("Unauthorised User")

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

        val err = request.flash.get("ERROR").getOrElse("")
        val comment = request.flash.get("commentText").getOrElse("")

        tsk.key match {
          case "assessEligibility" =>
            Future(Ok(views.html.assessEligibility(tsk, appFrontEndUrl, technologyMap, submitStatusMap, Some(userId))
            (Flash(Map("error" -> err, "comment" -> comment))) ))
          case "assignAssessors" =>
            Future(Ok(views.html.assignAssessors(tsk, appFrontEndUrl, getMembers(grp), List(), Some(userId))
            (Flash(Map("error" -> err, "comment" -> comment))) ))
          //case "firstAssessment" | "secondAssessment" | "thirdAssessment"=>
          case "firstAssessment"=>
            val errorCommentsMinLength = request.flash.get("errorCommentsMinLength").getOrElse("")
            val errorCommentsListMinLength = if(StringUtils.isNotEmpty(errorCommentsMinLength)) errorCommentsMinLength.split(",").toList else List()
            val errorCommentsMaxLength = request.flash.get("errorCommentsMaxLength").getOrElse("")
            val errorCommentsListMaxLength = if(StringUtils.isNotEmpty(errorCommentsMaxLength)) errorCommentsMaxLength.split(",").toList else List()
            Future(Ok(views.html.assessment(tsk, appFrontEndUrl, yesnoMap, scoreMap, "1", Some(errorCommentsListMinLength),
              Some(errorCommentsListMaxLength), Some(userId))
            (Flash(Map("error" -> err))) ))
          case "secondAssessment"=>
            val errorCommentsMinLength = request.flash.get("errorCommentsMinLength").getOrElse("")
            val errorCommentsListMinLength = if(StringUtils.isNotEmpty(errorCommentsMinLength)) errorCommentsMinLength.split(",").toList else List()
            val errorCommentsMaxLength = request.flash.get("errorCommentsMaxLength").getOrElse("")
            val errorCommentsListMaxLength = if(StringUtils.isNotEmpty(errorCommentsMaxLength)) errorCommentsMaxLength.split(",").toList else List()
            Future(Ok(views.html.assessment(tsk, appFrontEndUrl, yesnoMap, scoreMap, "2", Some(errorCommentsListMinLength),
              Some(errorCommentsListMaxLength), Some(userId))
            (Flash(Map("error" -> err))) ))
          case "thirdAssessment"=>
            val errorCommentsMinLength = request.flash.get("errorCommentsMinLength").getOrElse("")
            val errorCommentsListMinLength = if(StringUtils.isNotEmpty(errorCommentsMinLength)) errorCommentsMinLength.split(",").toList else List()
            val errorCommentsMaxLength = request.flash.get("errorCommentsMaxLength").getOrElse("")
            val errorCommentsListMaxLength = if(StringUtils.isNotEmpty(errorCommentsMaxLength)) errorCommentsMaxLength.split(",").toList else List()
            Future(Ok(views.html.assessment(tsk, appFrontEndUrl, yesnoMap, scoreMap, "3", Some(errorCommentsListMinLength),
              Some(errorCommentsListMaxLength), Some(userId))
            (Flash(Map("error" -> err))) ))
          case "makePanelDecision" =>
            Future(Ok(views.html.makePanelDecision(tsk, appFrontEndUrl, decisionMap, Some(userId))
            (Flash(Map("error" -> err, "comment" -> comment))) ))
          case "moderateScore" =>
            Future(Ok(views.html.moderateScore(tsk, appFrontEndUrl, decisionMap, Some(userId))
            (Flash(Map("error" -> err, "comment" -> comment))) ))
          case "confirmEmailSent" =>
            Future(Ok(views.html.confirmEmailSent(tsk, appFrontEndUrl, Some(userId))
            (Flash(Map("error" -> err, "comment" -> comment))) ))
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

   val userId = request.session.get("username").getOrElse("Unauthorised User")
    val ts = localtasks.showTasks(UserId(userId))

    ts.flatMap{
      case ts =>
        sortstr match {
          case "task-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.key), Some(userId))))
          case "task-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.key).reverse, Some(userId) )))
          case "app-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.appRef), Some(userId))))
          case "app-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.appRef).reverse, Some(userId) )))
          case "technology-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.technology), Some(userId))))
          case "technology-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.technology).reverse, Some(userId) )))
          case "aws-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averageweightedscore), Some(userId))))
          case "aws-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averageweightedscore).reverse, Some(userId) )))
          case "atbs-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averagetiebreakscore), Some(userId))))
          case "atbs-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.averagetiebreakscore).reverse, Some(userId) )))
          case "status-asc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.status), Some(userId))))
          case "status-desc" =>
            Future (Ok(views.html.tasks(ts.sortBy(_.status).reverse, Some(userId) )))
          case _ =>
            Future (Ok(views.html.tasks(ts, Some(userId))))
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
    val applicationId =     getValueFromRequest("applicationId", mp )
    val opportunityId =     getValueFromRequest("opportunityId", mp )

    if(commentMaxLengthCheck(comment, 1000))
      Future(Redirect(controllers.routes.TaskController.task(id, applicationId.toLong, opportunityId.toLong))
        .flashing("ERROR" -> Messages("error.BF008"), "commentText" -> comment))
    else {
      localtasks.submitEligibility(id, UserId(userId), status, comment, technology, processInstanceId).map {
        case Some(t) => {
          val ts = localtasks.showTasks(UserId(userId))
          Redirect(controllers.routes.TaskController.tasks())
        }
        case _ => NoContent

      }
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

    val applicationId =     getValueFromRequest("applicationId", mp )
    val opportunityId =     getValueFromRequest("opportunityId", mp )

    val errors = duplicateSelectionCheck(assignassessor1, assignassessor2, assignassessor3).fold(_.toList, _ => List())

    if(commentMaxLengthCheck(comment, 1000))
      Future(Redirect(controllers.routes.TaskController.task(id, applicationId.toLong, opportunityId.toLong))
        .flashing("ERROR" -> Messages("error.BF008"), "commentText" -> comment))
    else {
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
              Future.successful(Ok(views.html.assignAssessors(lt, appFrontEndUrl, getMembers(grp), errors)
              (Flash(Map("comment" -> comment))) ))
            case None =>
              Future.successful(NotFound)
          }
      }
    }
  }


  def submitAssessment (id : LocalTaskId, key: String) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")

    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val projectdesc =                  getValueFromRequest("projectdescriptionoption", mp )
    val projectdesccomment =           getValueFromRequest("projectdescriptioncomment", mp )
    val projectdescweight =            getValueFromRequest("projectdescweight", mp ).toInt

    val cost =                         getValueFromRequest("costoption", mp ).toInt
    val costcomment =                  getValueFromRequest("costcomment", mp )
    val costweight =                   getValueFromRequest("costweight", mp ).toInt

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

    val processInstanceId =            getValueFromRequest("processInstanceId", mp )
    val applicationId =                getValueFromRequest("applicationId", mp )
    val opportunityId =                getValueFromRequest("opportunityId", mp )

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
    val costScore = cost * costweight / 100.0
    val projectdeliveryScore = projectdelivery * projectdeliveryweight / 100.0
    val projectfinancingScore = projectfinancing * projectfinancingweight / 100.0

    /* Wider objective - Tie Breaker only - Dont add it now */
    val widerobjectiveScore = widerobjective * widerobjectiveweight / 100.0

    val weightedScore = performanceenhancementScore + costScore+ performancemanagementScore + performanceintegrationScore + marketpotentialScore
                        + projectdeliveryScore + projectfinancingScore

    val tiebreakScore = weightedScore + widerobjectiveScore


    /*println("=== score " + "" + "==  " + performanceenhancement)
    println("=== score " + "" + "==  " + performancemanagement)
    println("=== score " + "" + "==  " + performanceintegration)
    println("=== score " + "" + "==  " + cost)
    println("=== score " + "" + "==  " + marketpotential)
    println("=== score " + "" + "==  " + projectdelivery)
    println("=== score " + "" + "==  " + projectfinancing)
    println("=== score " + "" + "==  " + widerobjective)

    println("=== score weight" + "" + "==  " + performanceenhancementweight)
    println("=== score weight" + "" + "==  " + performancemanagementweight)
    println("=== score weight" + "" + "==  " + performanceintegrationweight)
    println("=== score weight" + "" + "==  " + marketpotentialweight)
    println("=== score weight" + "" + "==  " + costweight)
    println("=== score weight" + "" + "==  " + projectdeliveryweight)
    println("=== score weight" + "" + "==  " + projectfinancingweight)
    println("=== score weight" + "" + "==  " + widerobjectiveweight)



    println("=== score Score" + "" + "==  " + performanceenhancementScore)
    println("=== score Score" + "" + "==  " + performancemanagementScore)
    println("=== score Score" + "" + "==  " + performanceintegrationScore)
    println("=== score Score" + "" + "==  " + costScore)
    println("=== score Score" + "" + "==  " + marketpotentialScore)
    println("=== score Score" + "" + "==  " + projectdeliveryScore)
    println("=== score Score" + "" + "==  " + projectfinancingScore)
    println("=== score Score" + "" + "==  " + widerobjectiveScore)
    println("=== weightedScore " + "" + "==  " + weightedScore)
    println("=== tiebreakScore " + "" + "==  " + tiebreakScore)*/


    val score = Score(
      projectdesc, projectdesccomment, projectdescweight.toInt,
      cost.toInt, costcomment, costweight.toInt,
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

    val commentList =  List(
      ("Project description comment", projectdesccomment), ("Cost comment", costcomment), ("Performance comment", performancecomment),
      ("Market potential comment", marketpotentialcomment),  ("Project delivery comment", projectdeliverycomment),
      ("Project financing comment", projectfinancingcomment), ("Wider objective comment", widerobjectivecomment),
      ("Overall comment", overallcomment)
    )

    val errorComments_ = commentList.foldLeft(List[String]()) { (z,f) =>
      if(commentMaxLengthCheck(f._2, 1000)) z :+ f._1
      else z
    }

    val errorCommentsMinLength = commentList.foldLeft("") { (z,f) =>
      val sep = if(StringUtils.isNotEmpty(z)) "," else ""
      if(commentMinLengthCheck(f._2)) s"$z$sep${f._1}"
      else z
    }

    val errorCommentsMaxLength = commentList.foldLeft("") { (z,f) =>
      val sep = if(StringUtils.isNotEmpty(z)) "," else ""
      if(commentMaxLengthCheck(f._2, 1000)) s"$z$sep${f._1}"
      else z
    }

    if(StringUtils.isNotEmpty(errorCommentsMinLength) || StringUtils.isNotEmpty(errorCommentsMaxLength)) {
      localtasks.submitAssessment(id, UserId(userId), asmtKey, score, processInstanceId, "save").map {
        case _ => NoContent
      }
      Future(Redirect(controllers.routes.TaskController.task(id, applicationId.toLong, opportunityId.toLong))
        .flashing("errorCommentsMinLength" -> errorCommentsMinLength, "errorCommentsMaxLength" -> errorCommentsMaxLength))
    }
    else {
      localtasks.submitAssessment(id, UserId(userId), asmtKey, score, processInstanceId, button_action).map {
        case Some(t) => {
          val ts = localtasks.showTasks(UserId(userId))
          Redirect(controllers.routes.TaskController.tasks())
        }
        case _ => NoContent
      }
    }
  }

  def commentMinLengthCheck(comment:String): Boolean =
  //StringUtils.isEmpty(comment.replaceAll("\\s+", ""))
    StringUtils.isEmpty(comment)


  def commentMaxLengthCheck(comment:String, maxAllowed: Int): Boolean =
      (StringUtils.isNotEmpty(comment) && comment.split(" ").toList.size > maxAllowed)

  def getValueFromRequest(key: String, keyValueMap: Map[String, Seq[String]]): String =

    keyValueMap.get(key).headOption.map(_.head).getOrElse("").toString


  def submitMakePanelDecision (id : LocalTaskId) = Action.async { implicit request =>

    val userId = request.session.get("username").getOrElse("Unauthorised User")
    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val status =            getValueFromRequest("approvestatus", mp )
    val comment =           getValueFromRequest("comment", mp )
    val processInstanceId = getValueFromRequest("processInstanceId", mp )
    val applicationId =     getValueFromRequest("applicationId", mp )
    val opportunityId =     getValueFromRequest("opportunityId", mp )

    if(commentMaxLengthCheck(comment, 1000))
    Future(Redirect(controllers.routes.TaskController.task(id, applicationId.toLong, opportunityId.toLong))
      .flashing("ERROR" -> Messages("error.BF008"), "commentText" -> comment))
    else {
      localtasks.submitMakePanelDecision(id, UserId(userId), status, comment, processInstanceId).flatMap {
        case Some(t) => {
          val ts = localtasks.showTasks(UserId(userId))
          Future.successful(Redirect(controllers.routes.TaskController.tasks()))
        }
        case _ => Future.successful(NotFound)
      }
    }
  }


  def submitModerateScore (id : LocalTaskId) = Action.async { implicit request =>
    val userId = request.session.get("username").getOrElse("Unauthorised User")
    val mp = request.body.asFormUrlEncoded.getOrElse(Map())

    val averagemoderatescore =     getValueFromRequest("averagemoderatescore", mp )
    val comment =     getValueFromRequest("comment", mp )
    val processInstanceId =     getValueFromRequest("processInstanceId", mp )

    val applicationId =     getValueFromRequest("applicationId", mp )
    val opportunityId =     getValueFromRequest("opportunityId", mp )

    if(commentMaxLengthCheck(comment, 1000))
      Future(Redirect(controllers.routes.TaskController.task(id, applicationId.toLong, opportunityId.toLong))
        .flashing("ERROR" -> Messages("error.BF008"), "commentText" -> comment))
    else {
      localtasks.submitModerateScore(id, UserId(userId), averagemoderatescore, comment, processInstanceId).flatMap {
        case Some(t) => {
          val ts = localtasks.showTasks(UserId(userId))
          Future.successful(Redirect(controllers.routes.TaskController.tasks()))
        }
        case None => Future.successful(NotFound)
      }
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

    val applicationId =     getValueFromRequest("applicationId", mp )
    val opportunityId =     getValueFromRequest("opportunityId", mp )

    if(commentMaxLengthCheck(comment, 1000))
      Future(Redirect(controllers.routes.TaskController.task(id, applicationId.toLong, opportunityId.toLong))
        .flashing("ERROR" -> Messages("error.BF008"), "commentText" -> comment))
    else {
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
    println("=== In Task Controller == cost" + ":" + "              :-" + score.cost)
    println("=== In Task Controller == costcomment" + ":" + "       :-" + score.costcomment)
    println("=== In Task Controller == costweight" + ":" + "        :-" + score.costweight)
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
    println("=== In Task Controller == weightedscore" + ":" + "     :-" + score.weightedscore)
    println("=== In Task Controller == tiebreakscore" + ":" + "     :-" + score.tiebreakscore)
  }
}


