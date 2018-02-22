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

package services

import java.text.SimpleDateFormat

import com.google.inject.Inject
import models.{ApplicationId, ProcessInstanceSummary, UserId, _}
import org.activiti.engine._
import org.activiti.engine.impl.cfg.StandaloneProcessEngineConfiguration
import org.activiti.engine.task.{Comment, IdentityLink, Task, TaskQuery}
import config.Config
import org.activiti.engine.history.{HistoricProcessInstance, HistoricTaskInstance, HistoricVariableInstance, HistoricVariableInstanceQuery}
import org.activiti.engine.identity.{GroupQuery, UserQuery}
import org.activiti.engine.impl.persistence.entity.VariableInstance
import org.joda.time.LocalDateTime
//import org.activiti.engine.runtime.ProcessInstance
import org.joda.time.{DateTime, DateTimeZone}
import play.api.Logger
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import services.RestService.{JsonParseException, RestFailure}

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.JavaConversions._
import scala.util.Try
import org.apache.commons.lang3.StringUtils

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}
import validations.FieldError


class ApplicationURLs(appBaseUrl: String) {

  /** Application DB URL **/
  def appStatus(id: ApplicationId) =
    s"$appBaseUrl/application/${id.id}/appstatus"

  def updateMessage(id: ApplicationId) =
    s"$appBaseUrl/message/${id.id}/messageboard"

  def addMessage() =
    s"$appBaseUrl/message/messageboard"

  def applicationSectionData(id: ApplicationId, sectionNumber: AppSectionNumber) =
    s"$appBaseUrl/application/${id.id}/section/${sectionNumber.num}"


}

class BEISTaskService @Inject()(val ws: WSClient)(implicit val ec: ExecutionContext)
  extends BEISTaskOps with RestService {

  private val authorizationHeader = "Authorization"

  val appBackEndUrl = Config.config.business.appBackEndUrl
  val urls = new ApplicationURLs(appBackEndUrl)
  implicit val applicationIdReads = Json.reads[ApplicationId]
  implicit val messageIdReads = Json.reads[MessageId]
  implicit val userIdFormat = Json.format[UserId]
  implicit val ApplicationIdFormat = Json.format[ApplicationId]
  implicit val messageIdFormat = Json.format[MessageId]
  implicit val messageFormat = Json.format[Message]

  //implicit val localDateTimeFormat = Json.format[LocalDateTime]
  implicit val appSectionNumberFormat = Json.format[AppSectionNumber]
  implicit val applicationSectionFormat = Json.format[ApplicationSection]



  val taskService = ActivitiService.taskService
  val processEngine = ActivitiService.processEngine
  val historyService = ActivitiService.historyService
  val runtimeService = ActivitiService.runtimeService
  val identityService = ActivitiService.identityService


  override def showTask(id: LocalTaskId): Future[Option[LocalTask]] = {
    import collection.JavaConverters._

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val lng:Long = 0

    val v = taskService.getIdentityLinksForTask(t.getId)
    val  groupOrUser = v.foldLeft(List[String]()) { (z,l) =>
      if(l.getGroupId != null)
        z:+ s"${l.getGroupId}"
      else
        z:+ s"${l.getUserId}"
    }
    val taskInstanceHistoryList: Seq[HistoricTaskInstance] = historyService.createHistoricTaskInstanceQuery()
      .processInstanceId(t.getProcessInstanceId).orderByHistoricTaskInstanceStartTime().asc()
      .list()

    /* Build Task history with TaskHistory API*/
    val tskHistories: Seq[TaskHistory] = taskInstanceHistoryList.map{ tk =>

      TaskHistory(tk.getName,
        Try(java.lang.String.valueOf(historyService.createHistoricVariableInstanceQuery().taskId(tk.getId).variableName("completedby").singleResult().getValue())).toOption match {
          case Some(s) => s
          case _ => "-"
        },
        new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(tk.getStartTime),
        if(tk.getEndTime == null) Option("Not completed") else Option(new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(tk.getEndTime)),
        Try(java.lang.String.valueOf(historyService.createHistoricVariableInstanceQuery().taskId(tk.getId).variableName("approvestatus").singleResult().getValue())).toOption match {
          case Some(s) => s.capitalize
          case _ => "In Progress"
        },
        taskService.getTaskComments(tk.getId).map{ a=>LocalComment(new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(a.getTime), a.getFullMessage)
        }
      )
    }

    val key = t.getTaskDefinitionKey()

    val formData = Option(processEngine.getTaskService.getVariableInstances(t.getId)) match {
      case None => Map[String, String]()
      case Some(s) =>
        val weightedscore1 = Option(s.get("weightedscore1")) match { case Some(t) => t.getValue.toString.toDouble case None => 0.00 }
        val weightedscore2 = Option(s.get("weightedscore2")) match { case Some(t) => t.getValue.toString.toDouble case None => 0.00 }
        val weightedscore3 = Option(s.get("weightedscore3")) match { case Some(t) => t.getValue.toString.toDouble case None => 0.00 }

        val averageWeightedscore = (weightedscore1 + weightedscore2 + weightedscore3) / 3

        val tiebreakscore1 = Option(s.get("tiebreakscore1")) match { case Some(t) => t.getValue.toString.toDouble case None => 0.00 }
        val tiebreakscore2 = Option(s.get("tiebreakscore2")) match { case Some(t) => t.getValue.toString.toDouble case None => 0.00 }
        val tiebreakscore3 = Option(s.get("tiebreakscore3")) match { case Some(t) => t.getValue.toString.toDouble case None => 0.00 }
        val averageTiebreakscore = (tiebreakscore1 + tiebreakscore2 + tiebreakscore3) / 3

         s.map{
            case (k,v) => (k,
              Try(v.getValue) match{
                case Failure(e) => 0.asInstanceOf[AnyRef]
                case Success(v) => v
              }
            )
        } +=  (
          "averageweightedscore" -> BigDecimal(averageWeightedscore).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble.asInstanceOf[AnyRef],
          "averagetiebreakscore" -> BigDecimal(averageTiebreakscore).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble.asInstanceOf[AnyRef]
          )
    }

    val appId: Long = getVariable[Long](runtimeService, t.getProcessInstanceId, "ApplicationId", lng)
    val status: String = getVariable[String](runtimeService, t.getProcessInstanceId, "approvestatus", "-").capitalize
    val applicant: String = getVariable[String](runtimeService, t.getProcessInstanceId, "Applicant", "-")
    val appRef: String = getVariable[String](runtimeService, t.getProcessInstanceId, "ApplicationReference", "0")
    val oppId: Long = getVariable[Long](runtimeService, t.getProcessInstanceId, "OpportunityId", lng)

    val oppTitle = processEngine.getRuntimeService().getVariable(t.getProcessInstanceId, "OpportunityTitle").toString

    Future.successful(Option(LocalTask(LocalTaskId(t.getId), key, t.getName, groupOrUser, UserId(applicant), status, appId, appRef, oppId, oppTitle, t.getDescription,
      ProcessDefinitionId(t.getProcessDefinitionId), ProcessInstanceId(t.getProcessInstanceId), tskHistories, formData.toMap
    )))
  }

  override def getMembers(groupIds: Seq[String]): Option[Map[String,String]] = {

    val policyadmingroup = Config.config.bpm.policyadmingroup
    val userQuery: UserQuery = identityService.createUserQuery().memberOfGroup(policyadmingroup)

    val userNames = groupIds.foldLeft(Map[String,String]()) { (z,l) =>
      z ++
      identityService.createUserQuery().memberOfGroup(l).list().foldLeft(Map[String, String]()){ (a,b) =>
        a +  (b.getId -> s"${b.getFirstName} ${b.getLastName}")
      }
    }
    Option(userNames.toMap)
  }

  override def showTasks(userId: Option[UserId]): Future[Seq[LocalTaskSummary]] = {
    import collection.JavaConverters._
    val tasks = userId match {
      case None =>
        /*taskService.createTaskQuery().list().map{ts=>
          val v = taskService.getIdentityLinksForTask(ts.getId)
          val groups = v.foldLeft(List[String]()) { (z,l) =>
            z:+ s"${l.getGroupId}"
          }
        }*/
        taskService.createTaskQuery().list()

      case _ =>
        taskService.createTaskQuery().taskCandidateOrAssigned(userId.get.userId).list()
    }

    val tasksummaries = tasks.map(t => {
      val aws = getVariable[Double](runtimeService, t.getProcessInstanceId, "averageweightedscore", 0)
      val ams = getVariable[Double](runtimeService, t.getProcessInstanceId, "averagemoderatescore", 0)
      val v = taskService.getIdentityLinksForTask(t.getId)
      val lng:Long = 0

      val  groupOrUser:List[String] = v.foldLeft(List[String]()) { (z,l) =>
        if(l.getGroupId != null)
          z:+ s"${l.getGroupId}"
        else
          z:+ s"${l.getUserId}"
      }

      LocalTaskSummary(
        LocalTaskId(t.getId),
        t.getName,
        t.getTaskDefinitionKey(),
        if(StringUtils.isEmpty(t.getAssignee)) groupOrUser.headOption.getOrElse("") else t.getAssignee,
        UserId(getVariable[String](runtimeService, t.getProcessInstanceId, "Applicant", "")),
        getVariable[String](runtimeService, t.getProcessInstanceId, "approvestatus", "Not set").capitalize,
        getVariable[Long](runtimeService, t.getProcessInstanceId, "ApplicationId", lng),
        getVariable[String](runtimeService, t.getProcessInstanceId, "ApplicationReference", "Not set"),
        getVariable[Long](runtimeService, t.getProcessInstanceId, "OpportunityId", lng),
        getVariable[String](runtimeService, t.getProcessInstanceId, "technology", "Not set"),
        //(getVariable[Double](runtimeService, t.getProcessInstanceId, "averageweightedscore", 0) == 0) ? 0 : getVariable[Double](runtimeService, t, "averagemoderatescore", 0),
        if( ams > 0) ams else if(aws > 0) aws else 0,
        getVariable[Double](runtimeService, t.getProcessInstanceId, "averagetiebreakscore", 0))

    }).toSeq
    Future.successful(tasksummaries)
  }

  override def showProcessSummaries(userId: UserId): Future[Seq[ProcessInstanceSummary]] = {
    import collection.JavaConverters._

    val processInstances = historyService.createHistoricProcessInstanceQuery().list()
    val processInstanceSummaries_ = processInstances.map { p =>
    val historyVariableQuery = historyService.createHistoricVariableInstanceQuery()

      val processInstanceId = p.getId

      val applicationId =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationId").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationId").list().last.getValue
      else "0"
      val appRef =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationReference").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationReference").list().last.getValue
      else "NA"
      val applicant =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("Applicant").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("Applicant").list().last.getValue
      else "NA"

      val approvestatus =
        if(!(getVariable[String](runtimeService, processInstanceId, "approvestatus", "-").equals("-")) )
          getVariable[String](runtimeService, processInstanceId, "approvestatus", "-")
        else if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("approvestatus").list().size() > 0)
          historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("approvestatus").list().last.getValue
        else "NA"

      val averageweightedscore = java.lang.Double.valueOf(getHistoryVariable[String](historyService, processInstanceId, "averageweightedscore", "0.0"))
      val averagetiebreakscore = java.lang.Double.valueOf(getHistoryVariable[String](historyService, processInstanceId, "averagetiebreakscore", "0.0"))

      val technology =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("technology").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("technology").list().last.getValue
      else "NA"

      ProcessInstanceSummary(processInstanceId, "name", applicant.toString, approvestatus.toString, applicationId.toString.toLong, appRef.toString,
        technology.toString, averageweightedscore.toString.toDouble, averagetiebreakscore.toString.toDouble)

    }.toSeq

    val processInstanceSummaries = processInstanceSummaries_.foldLeft(Seq[ProcessInstanceSummary]()) {(k, l) =>
      if(l.appId != null && l.appId > 0l )
        k ++ Seq(l)
      else
        k
    }

    Future.successful(processInstanceSummaries)
  }

  override def showProcesses(userId: UserId): Future[Seq[LocalProcessInstance]] = {

    import collection.JavaConverters._
    import scala.concurrent.Await
    import scala.concurrent.duration.DurationInt

    val historyVariableQuery = historyService.createHistoricVariableInstanceQuery()

    val processInstances = historyService.createHistoricProcessInstanceQuery().list()
    val processInstances_ = processInstances.map { p =>

      val processInstanceId = p.getId

      //val processDefinitionId = p.getProcessDefinitionId
      val applicationId =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationId").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationId").list().last.getValue
      else "0"
      val appRef =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationReference").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("ApplicationReference").list().last.getValue
      else "NA"
      val applicant =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("Applicant").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("Applicant").list().last.getValue
      else "NA"

      val approvestatus =
        if(!(getVariable[String](runtimeService, processInstanceId, "approvestatus", "-").equals("-")) )
          getVariable[String](runtimeService, processInstanceId, "approvestatus", "-")
        else if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("approvestatus").list().size() > 0)
          historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("approvestatus").list().last.getValue
        else "NA"

      val averageweightedscore = java.lang.Double.valueOf(getHistoryVariable[String](historyService, processInstanceId, "averageweightedscore", "0.0"))
      val averagetiebreakscore = java.lang.Double.valueOf(getHistoryVariable[String](historyService, processInstanceId, "averagetiebreakscore", "0.0"))
      val averagemoderatescore = java.lang.Double.valueOf(getHistoryVariable[String](historyService, processInstanceId, "averagemoderatescore", "0.0"))

      val technology =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("technology").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("technology").list().last.getValue
      else "NA"

      val assignee1 =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("assignee1").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("assignee1").list().last.getValue
      else "NA"

      val assignee2 =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("assignee2").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("assignee2").list().last.getValue
      else "NA"

      val assignee3 =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("assignee3").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("assignee3").list().last.getValue
      else "NA"

      val maxDeviation =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("maxDeviation").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("maxDeviation").list().last.getValue
      else "0"

      val assignee1Weightedscore =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("weightedscore1").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("weightedscore1").list().last.getValue
      else "0"

      val assignee2Weightedscore =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("weightedscore2").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("weightedscore2").list().last.getValue
      else "0"

      val assignee3Weightedscore =  if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("weightedscore3").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("weightedscore3").list().last.getValue
      else "0"

      /** Rest call to Front end to get the Application data
          Organisation: String, projectTitle: String, projectValue: String, grantValue: String **/

      val appdata = getApplicationSectionData(ApplicationId(applicationId.toString.toLong), AppSectionNumber(1) ).flatMap{
        case Some(s) =>
          println("===App data JSON===="+ s)

          Future.successful((
            (s.answers \ "proposalsummary" \ "name").validate[String].asOpt.getOrElse("NA"),
            (s.answers \ "proposalsummary" \ "title").validate[String].asOpt.getOrElse("NA"),
            (s.answers \ "proposalsummary" \ "costs").validate[String].asOpt.getOrElse("0"),
            (s.answers \ "proposalsummary" \ "totalcost").validate[String].asOpt.getOrElse("0") ))

        case None =>
          Future.successful(("NA", "NA", "0", "0"))
      }
      /** Rest call ends **/

      val organisation =  Await.result(appdata.map(j=>j._1.toString), 10.seconds)
      val projectTitle =  Await.result(appdata.map(j=>j._2.toString), 10.seconds)
      val projectValue =  Await.result(appdata.map(j=>j._3.toString.toLong), 10.seconds)
      val grantValue =    Await.result(appdata.map(j=>j._4.toString.toLong), 10.seconds)


      println("===organisation===="+ organisation)
      println("===projectTitle===="+ projectTitle)
      println("===projectValue===="+ projectValue)
      println("===grantValue===="+ grantValue)

      LocalProcessInstance(processInstanceId, "name", applicant.toString, approvestatus.toString, applicationId.toString.toLong, appRef.toString,
        organisation, projectTitle, projectValue, grantValue,
        technology.toString, maxDeviation.toString.toDouble,
        averageweightedscore.toString.toDouble, averagetiebreakscore.toString.toDouble, averagemoderatescore.toString.toDouble,
        assignee1.toString, assignee2.toString, assignee3.toString, assignee1Weightedscore.toString, assignee2Weightedscore.toString,
        assignee3Weightedscore.toString)

    }.toSeq

    val processInstancesRefined = processInstances_.foldLeft(Seq[LocalProcessInstance]()) {(k, l) =>
      if(l.appId != null && l.appId > 0l )
        k ++ Seq(l)
      else
        k
    }

    Future.successful(processInstancesRefined)
  }


  override def showProcess(id: ProcessId): Future[Option[LocalProcess]] = {
    import collection.JavaConverters._

    val historyVariableQuery = historyService.createHistoricVariableInstanceQuery()

    val p: HistoricProcessInstance = historyService.createHistoricProcessInstanceQuery().processInstanceId(id.id.toString()).singleResult()
    val processInstanceId = id.id.toString()

    val taskInstanceHistoryList: Seq[HistoricTaskInstance] = historyService.createHistoricTaskInstanceQuery()
      .processInstanceId(id.id.toString()).orderByHistoricTaskInstanceStartTime().asc()
      .list()

    /* Build Task history with TaskHistory API*/
    val tskHistories: Seq[TaskHistory] = taskInstanceHistoryList.map{ tk =>

      TaskHistory(tk.getName,
        Try(java.lang.String.valueOf(historyVariableQuery.taskId(tk.getId).variableName("completedby").singleResult().getValue())).toOption match {
          case Some(s) => s
          case _ => "-"
        },
        new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(tk.getStartTime),
        if(tk.getEndTime == null) Option("Not completed") else Option(new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(tk.getEndTime)),
        Try(java.lang.String.valueOf(historyVariableQuery.taskId(tk.getId).variableName("approvestatus").singleResult().getValue())).toOption match {
          case Some(s) => s.capitalize
          case _ => "In Progress"
        },
        taskService.getTaskComments(tk.getId).map{ a=>LocalComment(new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(a.getTime), a.getFullMessage)
        }
      )
    }

    val appId = java.lang.Long.valueOf(getHistoryVariable[String](historyService, processInstanceId, "ApplicationId", "0"))
    val applicant = getHistoryVariable[String](historyService, processInstanceId, "Applicant", "-")
    val appRef = getHistoryVariable[String](historyService, processInstanceId, "ApplicationReference", "-")
    val oppId = java.lang.Long.valueOf(getHistoryVariable[String](historyService, processInstanceId, "OpportunityId", "0"))
    val oppTitle = getHistoryVariable[String](historyService, processInstanceId, "OpportunityTitle", "-")

    val status =
      if(!(getVariable[String](runtimeService, processInstanceId, "approvestatus", "-").equals("-")) )
        getVariable[String](runtimeService, processInstanceId, "approvestatus", "-")
      else if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("approvestatus").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("approvestatus").list().last.getValue
      else "notset"

    val additionalInfo =
      if(!(getVariable[String](runtimeService, processInstanceId, "additionalInfo", "-").equals("-")) )
        getVariable[String](runtimeService, processInstanceId, "additionalInfo", "-")
      else if(historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("additionalInfo").list().size() > 0)
        historyService.createHistoricVariableInstanceQuery().processInstanceId(p.getId).variableName("additionalInfo").list().last.getValue
      else ""

    Future.successful(Option(LocalProcess(id, appId, appRef, oppId, oppTitle, status.toString, tskHistories, Some(additionalInfo.toString) )))
  }

  final def getVariable [T] (runtimeService: RuntimeService, processInstanceId: String, v: String, t: T ): T = {

    import scala.util.Try

    Try( Option(runtimeService.getVariable(processInstanceId, v))) match {

      case Success(s) =>

        (try{
          //val v = getType(s.getOrElse(t).asInstanceOf[AnyRef], t).asInstanceOf[T]
          getType(s.getOrElse(t).toString, t).asInstanceOf[T]
        } catch {
          case e: Exception => t
        })

      case Failure(er) => t
      case _ => t
    }
  }

  def getType [T] (s: String, t: T) = {

    t match {
      case a if(a.isInstanceOf[String]) => s
      case a if(a.isInstanceOf[Double]) => s.toDouble
      case a if(a.isInstanceOf[Long]) =>   s.toLong
      case a if(a.isInstanceOf[Int]) => s.toInt
    }
  }

  final def getHistoryVariable [T] (historyService: HistoryService, processInstanceId: String, v: String, t: T ): T = {

    import scala.util.Try

    Try( Option(historyService.createHistoricVariableInstanceQuery().processInstanceId(processInstanceId).variableName(v).list().last.getValue)) match {

      case Success(s) =>
        val ss=  if(StringUtils.isEmpty(s.getOrElse(t).toString)) "0" else s.getOrElse(t).toString

        (try{ getType(ss, t).asInstanceOf[T]
        } catch {
          case e: Exception => t
        })

      case Failure(er) =>   t
      case _ =>  t
    }
  }

  override def submitEligibility(id: LocalTaskId, userId: UserId, status: String, comment: String, technology: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    val sts = status match {
      case "eligible" => 1
      case "noteligible" => 0
      case _ => 0
    }

    status match {
      case "eligible" => None
      case "noteligible" =>
        taskService.setVariableLocal(t.getId(), "finaldecision", "Not eligible")
    }

    val ss = Int.box(sts)
    val stt = status match {
      case "noteligible" => ("finaldecision" -> "Not eligible")
      case _ => ("finaldecision" -> "")
    }

    val mp = Map("isEligible" -> ss, "technology" -> technology, "submiteligibilitycomment" -> comment) +  stt

    //val i: Object = ss
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    taskService.setVariableLocal(t.getId(), "isEligible", sts )
    taskService.setVariableLocal(t.getId(), "technology", technology )
    taskService.setVariableLocal(t.getId(), "submiteligibilitycomment", comment)

    taskService.setVariableLocal(t.getId(), "approvestatus", Completed.status) //History purpose
    taskService.setVariableLocal(t.getId(), "completedby", userId.userId)     //History purpose
    taskService.setVariableLocal(t.getId(), "comment", comment)               //History purpose

    runtimeService.setVariable(t.getProcessInstanceId,"technology",technology) //Process variable for TaskSummary

    val processStatus = status match { case "eligible" => Eligible.status
    case "noteligible" => NotEligible.status
    case _ => NotEligible.status}

    runtimeService.setVariable(t.getProcessInstanceId,"approvestatus",processStatus) //Process variable for TaskSummary

    taskService.complete(t.getId(), mp, false)

    /** Update Application DB - Update Application Status **/
    //val st = s"WIP"
    /*val stsc = "WIP"
    val s = s"$stsc (by $user)"*/

    updateAppStatus(ApplicationId(applicationId), processStatus ) //TODO:- Should we update Application status to show to user in BEIS forms?

    Future.successful(Option(LocalTaskId(t.getId)))
  }

  override def submitAssignAssessors(id: LocalTaskId, userId: UserId, assignassessor1: String, assignassessor2: String,
                                     assignassessor3: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    //val st = s"WIP"
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)

    taskService.setVariableLocal(t.getId(), "assignee1", assignassessor1)
    taskService.setVariableLocal(t.getId(), "assignee2", assignassessor2)
    taskService.setVariableLocal(t.getId(), "assignee3", assignassessor3)
    taskService.setVariableLocal(t.getId(), "approvestatus", Completed.status) //History purpose
    taskService.setVariableLocal(t.getId(), "completedby", userId.userId)     //History purpose
    taskService.setVariableLocal(t.getId(), "comment", comment)               //History purpose

    runtimeService.setVariable(t.getProcessInstanceId,"approvestatus", AssessorsAssigned.status) //Process variable for TaskSummary

    taskService.complete(t.getId(), Map("assignee1" -> assignassessor1, "assignee2" -> assignassessor2,
      "assignee3" -> assignassessor3, "assignassessorscomment" -> comment ), false)

    /** Update Application DB - Update Application Status **/
    //val stsc = "WIP"
    //val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), AssessorsAssigned.status ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }



  override def submitAssessment(id: LocalTaskId, userId: UserId, asmtKey: Int,  score:Score,
                                 processInstanceId: String, buttonAction: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    //val user = userId.userId
    //val st = s"WIP"
    /* Get assessors's name */

    val user = identityService.createUserQuery().userId(userId.userId).singleResult()



    val mp = Map(
      s"projectdesc$asmtKey" -> score.projectdesc.toString,
      s"projectdesccomment$asmtKey" -> score.projectdesccomment,
      s"projectdescweight$asmtKey" -> score.projectdescweight.toString,

      s"cost$asmtKey" -> score.cost.toString,
      s"costcomment$asmtKey" -> score.costcomment,
      s"costweight$asmtKey" -> score.costweight.toString,

      s"performanceenhancement$asmtKey" -> score.performanceenhancement.toString,
      s"performanceenhancementweight$asmtKey" -> score.performanceenhancementweight.toString,
      s"performancemanagement$asmtKey" -> score.performancemanagement.toString,
      s"performancemanagementweight$asmtKey" -> score.performancemanagementweight.toString,
      s"performanceintegration$asmtKey" -> score.performanceintegration.toString,
      s"performanceintegrationweight$asmtKey" -> score.performanceintegrationweight.toString,

      s"performancecomment$asmtKey" -> score.performancecomment,

      s"marketpotential$asmtKey" -> score.marketpotential.toString,
      s"marketpotentialcomment$asmtKey" -> score.marketpotentialcomment,
      s"marketpotentialweight$asmtKey" -> score.marketpotentialweight.toString,

      s"projectdelivery$asmtKey" -> score.projectdelivery.toString,
      s"projectdeliverycomment$asmtKey" -> score.projectdeliverycomment,
      s"projectdeliveryweight$asmtKey" -> score.projectdeliveryweight.toString,

      s"projectfinancing$asmtKey" -> score.projectfinancing.toString,
      s"projectfinancingcomment$asmtKey" -> score.projectfinancingcomment,
      s"projectfinancingweight$asmtKey" -> score.projectfinancingweight.toString,

      s"widerobjective$asmtKey" -> score.widerobjective.toString,
      s"widerobjectivecomment$asmtKey" -> score.widerobjectivecomment,
     // s"widerobjectiveweight$asmtKey" -> score.widerobjectiveweight.toString,

      s"overallcomment$asmtKey" -> score.overallcomment,
      s"weightedscore$asmtKey" -> score.weightedscore.toString,
      s"tiebreakscore$asmtKey" -> score.tiebreakscore.toString,
      s"assessor$asmtKey" -> (user.getFirstName() + " " + user.getLastName())



    )

    taskService.setVariablesLocal(t.getId(), mp)
    taskService.setVariableLocal(t.getId(), "approvestatus", Completed.status) //History purpose

    buttonAction match {
      //case "save" => taskService.saveTask(t)
      case "complete" =>
        taskService.setVariableLocal(t.getId(), "completedby", userId.userId) //History purpose
        val approvests = runtimeService.getVariable(t.getProcessInstanceId,"approvestatus")
/*
        val sts = approvests.toString match {
          case AssessorsAssigned.status =>  s"${Assessed.status} by Assessor $asmtKey"
          case _ => s"$approvests and $asmtKey"
        }
*/
        val sts = AssessorsAssigned.status

       runtimeService.setVariable(t.getProcessInstanceId,"approvestatus", sts ) //Process variable for TaskSummary
       taskService.complete(t.getId(), mp, false)
      case _ => None
    }

    /** Update Application DB - Update Application Status **/
    //val stsc = "WIP"
    //val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), AssessorsAssigned.status ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }

  override def submitModerateScore(id: LocalTaskId, userId: UserId, averageweightedscore: String, averagemoderatescore: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()

    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    //val st = s"WIP"

    val averagetiebreakscore = taskService.getVariable(t.getId(),"averagetiebreakscore")
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    taskService.setVariableLocal(t.getId(), "averageweightedscore", averageweightedscore )
    taskService.setVariableLocal(t.getId(), "averagemoderatescore", averagemoderatescore )
    taskService.setVariableLocal(t.getId(), "averagetiebreakscore", averagetiebreakscore )
    taskService.setVariableLocal(t.getId(), "moderatescorecomments", comment)

    taskService.setVariableLocal(t.getId(), "averageweightedscore", averageweightedscore) //History purpose
    taskService.setVariableLocal(t.getId(), "approvestatus", Completed.status)            //History purpose
    taskService.setVariableLocal(t.getId(), "completedby", userId.userId)                 //History purpose
    taskService.setVariableLocal(t.getId(), "comment", comment)                           //History purpose

    runtimeService.setVariable(t.getProcessInstanceId,"averageweightedscore",averageweightedscore) //Process variable for TaskSummary
    runtimeService.setVariable(t.getProcessInstanceId,"averagetiebreakscore",averagetiebreakscore) //Process variable for TaskSummary
    runtimeService.setVariable(t.getProcessInstanceId,"averagemoderatescore",averagemoderatescore) //Process variable for TaskSummary
    runtimeService.setVariable(t.getProcessInstanceId,"approvestatus", s"${Moderated.status}" )    //Process variable for TaskSummary

    taskService.complete(t.getId(), Map("averagemoderatescore" -> averagemoderatescore), false)

    /** Update Application DB - Update Application Status **/
    //val stsc = "WIP"
    //val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), AssessmentCompleted.status )

    Future.successful(Option(LocalTaskId(t.getId)))
  }


  override def submitMakePanelDecision(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    //val st = s"WIP"
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    taskService.setVariableLocal(t.getId(), "makepaneldecisioncomments", comment)
    taskService.setVariableLocal(t.getId(), "makepaneldecisioncomments", comment)
    taskService.setVariableLocal(t.getId(), "finaldecision", status)
    taskService.setVariableLocal(t.getId(), "approvestatus", Completed.status) //History purpose
    taskService.setVariableLocal(t.getId(), "completedby", userId.userId)     //History purpose
    taskService.setVariableLocal(t.getId(), "comment", comment)               //History purpose

    val sts = status match { case "approved" => Approved.status  case _ => NotApproved.status}

    runtimeService.setVariable(t.getProcessInstanceId,"approvestatus", sts ) //Process variable for TaskSummary

    taskService.complete(t.getId(), Map("makepaneldecisioncomments" -> comment, "finaldecision"-> status), false)

    /** Update Application DB - Update Application Status **/
    //val stsc = "WIP"
    //val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), sts ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }


  def submitConfirmEmailSent(id: LocalTaskId, userId: UserId, emailsent: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    val user = userId.userId

    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    taskService.setVariableLocal(t.getId(), "emailsent", emailsent )
    taskService.setVariableLocal(t.getId(), "submitconfirmemailsentcomment", comment)

    taskService.setVariableLocal(t.getId(), "approvestatus", Completed.status) //History purpose
    taskService.setVariableLocal(t.getId(), "completedby", userId.userId)      //History purpose
    taskService.setVariableLocal(t.getId(), "comment", comment)                //History purpose

    taskService.complete(t.getId(), Map("emailsent" -> emailsent, "submiteligibilitycomment" -> comment), false)

    /** Update Application DB - Update Application Status **/
    //val st = s"WIP"
    /*val stsc = "WIP"
    val s = s"$stsc (by $user)"*/
    updateAppStatus(ApplicationId(applicationId), Completed.status ) //TODO:- Should we update Application status to show to user in BEIS forms?

    Future.successful(Option(LocalTaskId(t.getId)))
  }


  override def submitLogin(userId: String, password: String): Future[Option[String]] = {

    val isUserAuthonticated = identityService.checkPassword(userId, password)
    val grp = identityService.createGroupQuery().groupMember(userId).list().headOption

    (isUserAuthonticated && !grp.isEmpty) match{
      case false =>
        Future.successful(None)
      case true =>
        val grpId = grp.get.getId
        Future.successful(Option(grpId))
    }
  }

  override def saveUser(userId: String, firstName: String, lastName: String, password: String, email: String): Future[Int] = {

    import org.activiti.engine.identity.User

    val user: User  = identityService.newUser(userId)
    user.setFirstName(firstName)
    user.setLastName(lastName)
    user.setEmail(email)
    user.setPassword(password)

    Try(identityService.saveUser(user)) match {
      case Success(s) => Future.successful(1)
      case Failure(e) => println(s"Error in saving password for $userId :-"+ e.getMessage)
                         Future.successful(0)
    }
  }


  override def updatePassword(userId: String, password: String, newpassword: String): Future[Int] = {

    import org.activiti.engine.identity.User
    import collection.JavaConverters._

    //Check current password is correct
    val isUserAuthonticated = identityService.checkPassword(userId, password)

    isUserAuthonticated match {
      case true  =>
        val user: User = identityService.createUserQuery().userId(userId).list().head
        user.setPassword(newpassword)
        Try(identityService.saveUser(user)) match {
          case Success(s) => Future.successful(1)
          case Failure(e) => println(s"Error in saving password for $userId :-"+ e.getMessage)
            Future.successful(0)
        }
      case false  =>
        Future.successful(2)
    }

  }

  override def submitProcess(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)

    status match {
      case "needmoreinfo" => {
        /** Need to update(complete) BPM Process with needmoreinfo  status
            Need to Update Application DB (for Status and Message Board message) **/
        val user = userId.userId
        val s = s"Need more Info"
        taskService.setVariableLocal(t.getId(), "approvestatus", "Waiting" )
        taskService.setVariableLocal(t.getId(), "comment", comment)
        updateAppStatus(ApplicationId(applicationId), s )
        taskService.complete(t.getId(), Map("approvestatus" -> status ), false)

        /** Update Application DB - Add message in MessageBoard **/
        if(!StringUtils.isEmpty(comment))
          saveMessageBoard(Message(MessageId(0), UserId(applicant), ApplicationId(applicationId), 0, userId, DateTime.now(DateTimeZone.UTC), Option(comment) ))
      }
      case _ => {
        /** Update Process DB **/
        val sts = status match {
          case "approved" => Approved.status
          case "reviewed" => Reviewed.status
          case "rejected" => Rejected.status
        }
        val stsc = sts.capitalize

        /** Submit Task in the Processflow **/
        taskService.setVariableLocal(t.getId,"approvestatus",
          sts match {
            case Reviewed.status => "Completed"
            case _ => sts.capitalize
          }
        )
        taskService.setVariableLocal(t.getId(), "comment", comment)
        taskService.complete(t.getId(), Map("approvestatus" -> sts ), false)
        /* Set the next Task to In progress */
        taskService.createTaskQuery().processInstanceId(t.getProcessInstanceId).list().map( a=>
          taskService.setVariableLocal(a.getId,"approvestatus", "In Progress" )
        )

        /** Update Application DB - Update Application Status **/
        val user = userId.userId
        val s = s"$stsc (by $user)"
        updateAppStatus(ApplicationId(applicationId), s )

        /** Update Application DB - Add message in MessageBoard **/
        if(!StringUtils.isEmpty(comment))
          saveMessageBoard(Message(MessageId(0), UserId(applicant), ApplicationId(applicationId), 0, userId, DateTime.now(DateTimeZone.UTC), Option(comment) ))
      }

    }
    Future.successful(Option(LocalTaskId(t.getId)))
  }


  override def submitReAssignAssessor(id: LocalTaskId, assignTo: String): Future[Option[LocalTaskId]] = {

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val v = taskService.getIdentityLinksForTask(t.getId)

    val  groupOrUser:List[String] = v.foldLeft(List[String]()) { (z,l) =>
      if(l.getGroupId != null)
        z:+ s"${l.getGroupId}"
      else
        z:+ s"${l.getUserId}"
    }

    /*** Todo:- need to change the Proces diagram to assign User to assignee instead of CandidateUser in the task details ***/

    /* Delete existing User/s as CandidateUser (as we assigned when the Task is progressing from Assign Assessors*/
    groupOrUser.map{ usr =>
      taskService.deleteCandidateUser(id.id, usr)
    }

    /* History purpose */
    t.getTaskDefinitionKey match{
      case "firstAssessment" => taskService.setVariableLocal(t.getId(), "assignee1", assignTo)
      case "secondAssessment" => taskService.setVariableLocal(t.getId(), "assignee2", assignTo)
      case "thirdAssessment" => taskService.setVariableLocal(t.getId(), "assignee3", assignTo)
    }

    /* Set User in Candidate User */
    taskService.setAssignee(id.id, assignTo)
    Future.successful(Option(LocalTaskId(t.getId)))

  }


  override def updateProcessVariable(pid: ProcessId, additionalInfo: String) = {
    runtimeService.setVariable(pid.id.toString(),"additionalInfo", additionalInfo )
  }


  override def updateAppStatus(id: ApplicationId, appStatus: String): Future[Option[ApplicationId]] ={
    postWithResult[ApplicationId, String](urls.appStatus(id), appStatus)
  }

  override def saveMessageBoard(message: Message): Future[Option[MessageId]] ={
    postWithResult[MessageId, Message](urls.addMessage(), message)
  }

  override def updateMessageBoard(id: ApplicationId, message: String): Future[Option[MessageId]] ={
    postWithResult[MessageId, String](urls.updateMessage(id), message)
  }

  override def getApplicationSectionData(id: ApplicationId, sectionNumber: AppSectionNumber): Future[Option[ApplicationSection]] =
    getOpt[ApplicationSection](urls.applicationSectionData(id, sectionNumber))

}

trait ActivitiServices {
  val processEngine: ProcessEngine
  val runtimeService:RuntimeService
  val taskService:TaskService
}

case object ActivitiService extends ActivitiServices{

    val jdbcUrl = Config.config.bpm.url
    val jdbcDriver = Config.config.bpm.driver
    val dbUser = Config.config.bpm.user
    val dbPwd = Config.config.bpm.pwd

    val prconfig :ProcessEngineConfiguration = new StandaloneProcessEngineConfiguration()
      .setJdbcUrl(jdbcUrl)
      .setJdbcUsername(dbUser)
      .setJdbcPassword(dbPwd)
      .setJdbcDriver(jdbcDriver)
      .setDatabaseSchemaUpdate(ProcessEngineConfiguration.DB_SCHEMA_UPDATE_TRUE)

  println("*** Calling Activiti Database ***" )
  val processEngine: ProcessEngine = prconfig.buildProcessEngine()
  val historyService:HistoryService = processEngine.getHistoryService()
  val runtimeService:RuntimeService = processEngine.getRuntimeService()
  val taskService:TaskService = processEngine.getTaskService()
  val identityService:IdentityService = processEngine.getIdentityService()
}


