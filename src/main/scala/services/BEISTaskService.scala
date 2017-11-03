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
import models.{ApplicationId, UserId, _}
import org.activiti.engine._
import org.activiti.engine.impl.cfg.StandaloneProcessEngineConfiguration
import org.activiti.engine.task.{Comment, IdentityLink, Task, TaskQuery}
import config.Config
import org.activiti.engine.history.HistoricTaskInstance
import org.activiti.engine.identity.UserQuery
import org.activiti.engine.runtime.ProcessInstance
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

class ApplicationURLs(appBaseUrl: String) {

  /** Application DB URL **/
  def appStatus(id: ApplicationId) =
    s"$appBaseUrl/application/${id.id}/appstatus"

  def updateMessage(id: ApplicationId) =
    s"$appBaseUrl/message/${id.id}/messageboard"

  def addMessage() =
    s"$appBaseUrl/message/messageboard"
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


  override def showTask(id: LocalTaskId): Future[Option[LocalTask]] = {
    import collection.JavaConverters._

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val historyService:HistoryService = processEngine.getHistoryService()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()

    val v = taskService.getIdentityLinksForTask(t.getId)
    val groups = v.foldLeft(List[String]()) { (z,l) =>
      println("===users===" + l.getUserId)
      if(l.getGroupId != null)
        z:+ s"${l.getGroupId}"
      else
        z:+ s"${l.getUserId}"
    }
    //println("====   groups======" + groups.mkString(", "))

    val taskInstanceHistoryList: Seq[HistoricTaskInstance] = historyService.createHistoricTaskInstanceQuery()
      .processInstanceId(t.getProcessInstanceId).orderByHistoricTaskInstanceStartTime().asc()
      .list()

    /* Build Task history with TaskHistory API*/
    val tskHistories: Seq[TaskHistory] = taskInstanceHistoryList.map{ tk =>
      historyService.createHistoricTaskInstanceQuery().taskId(tk.getId).list()

      historyService.createHistoricVariableInstanceQuery()
        .taskId(tk.getId)
        .list()

      TaskHistory(tk.getName, tk.getAssignee, new SimpleDateFormat("dd-MMMM-yyyy HH:mm").format(tk.getStartTime),
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

    val formData = Option(processEngine.getTaskService.getVariable(t.getId, "formData")) match {
      case None => Map[String, AnyRef]()
      case Some(s) => s.asInstanceOf[Map[String, AnyRef]]
    }

    val appId: Long = getVariable[String](runtimeService, t, "ApplicationId", "0").toLong
    val status: String = getVariable[String](runtimeService, t, "approvestatus", "-").capitalize
    val applicant: String = getVariable[String](runtimeService, t, "Applicant", "-")
    val appRef: String = getVariable[String](runtimeService, t, "ApplicationReference", "0")
    val oppId: Long = getVariable[String](runtimeService, t, "OpportunityId", "0").toLong


    val oppTitle = processEngine.getRuntimeService().getVariable(t.getProcessInstanceId, "OpportunityTitle").toString

    Future.successful(Option(LocalTask(LocalTaskId(t.getId), key, t.getName, groups, UserId(applicant), status, appId, appRef, oppId, oppTitle, t.getDescription,
      ProcessDefinitionId(t.getProcessDefinitionId), ProcessInstanceId(t.getProcessInstanceId), tskHistories, formData
    )))
  }


  override def getMembers(groupIds: Seq[String]): Option[Set[String]] = {

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val identityService:IdentityService = processEngine.getIdentityService()
    val userQuery: UserQuery = identityService.createUserQuery().memberOfGroup("policyadmin")

    val userNames = groupIds.foldLeft(List[String]()) { (z,l) =>
       z:::identityService.createUserQuery().memberOfGroup(l).list().foldLeft(List[String]()){ (a,b) =>
        a :+ b.getId
      }
    }
    Option(userNames.toSet)
  }


  override def showTasks(userId: UserId): Future[Seq[LocalTaskSummary]] = {
    import collection.JavaConverters._
    val processEngine = ActivitiTaskService.apply()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()
    val tasks = userId   match {
      case UserId("admin") =>
        taskService.createTaskQuery().list().map{ts=>
           val v = taskService.getIdentityLinksForTask(ts.getId)
          val groups = v.foldLeft(List[String]()) { (z,l) =>
            z:+ s"${l.getGroupId}"
          }
        }
        taskService.createTaskQuery().list()

      case _ =>
        taskService.createTaskQuery().taskCandidateUser(userId.userId).list()
    }

    val tasksummaries = tasks.map(t => {
      println("===getTaskDefinitionKey===== "+ t.getTaskDefinitionKey())
      LocalTaskSummary(
        LocalTaskId(t.getId),
        t.getName,
        t.getTaskDefinitionKey(),
        UserId(getVariable[String](runtimeService, t, "Applicant", "")),
        getVariable[String](runtimeService, t, "approvestatus", "Not set").capitalize,
        getVariable[String](runtimeService, t, "ApplicationId", "0").toLong,
        getVariable[String](runtimeService, t, "ApplicationReference", "Not set"),
        getVariable[String](runtimeService, t, "OpportunityId", "0").toLong)
    }).toSeq
    Future.successful(tasksummaries)
  }

  final def getVariable [T] (runtimeService: RuntimeService, task: Task, v: String, t: T ): T = {

    Try((runtimeService.getVariable(task.getProcessInstanceId, v).asInstanceOf[AnyRef]).asInstanceOf[T] ).toOption match {
      case Some(s) => s
      case _ => t
    }
  }

  override def submitEligibility(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    val ss = Int.box(1)
    //val i: Object = ss
    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    val st = s"WIP"
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    taskService.setVariableLocal(t.getId(), "isEligible", 1 )
    taskService.setVariableLocal(t.getId(), "comment", comment) //TODO:- need to delete it if the process vaiable is not sed else where
    taskService.complete(t.getId(), Map("isEligible" -> ss ), false)

    /** Update Application DB - Update Application Status **/
    val stsc = "WIP"
    val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), st ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }

  override def submitAssignAssessors(id: LocalTaskId, userId: UserId, assignassessor1: String, assignassessor2: String,
                            assignassessor3: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    val st = s"WIP"
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    taskService.setVariableLocal(t.getId(), "comment", comment) //TODO:- need to delete it if the process vaiable is not sed else where

      taskService.setVariableLocal(t.getId(), "assignee1", assignassessor1)
      taskService.setVariableLocal(t.getId(), "assignee2", assignassessor2)
      taskService.setVariableLocal(t.getId(), "assignee3", assignassessor3)
      taskService.complete(t.getId(), Map("assignee1" -> assignassessor1, "assignee2" -> assignassessor2,
                       "assignee3" -> assignassessor3 ), false)

    /** Update Application DB - Update Application Status **/
    val stsc = "WIP"
    val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), st ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }

  override def  submitAssessment(id: LocalTaskId, userId: UserId, asmtKey: Int,  score:Score,
                                processInstanceId: String, buttonAction: String): Future[Option[LocalTaskId]] = {

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

      /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    val st = s"WIP"

    val mp = Map(
      s"projectdesc$asmtKey" -> score.projectdesc.toString,
      s"projectdesccomment$asmtKey" -> score.projectdesccomment,
      s"projectdescweight$asmtKey" -> score.projectdescweight.toString,

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

      s"widerobj$asmtKey" -> score.widerobj.toString,
      s"widerobjcomment$asmtKey" -> score.widerobjcomment,
      s"widerobjweight$asmtKey" -> score.widerobjweight.toString,

      s"overallcomment$asmtKey" -> score.overallcomment
    )

    taskService.setVariableLocal(t.getId(), "formData", mp)

    buttonAction match {
      //case "save" => taskService.saveTask(t)
      case "complete" => taskService.complete(t.getId(), mp, false)
      case _ => None
    }

    /** Update Application DB - Update Application Status **/
    val stsc = "WIP"
    val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), st ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }


   override def submitMakePanelDecision(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()

    val t: Task = taskService.createTaskQuery().taskId(id.id).singleResult()
    val applicationId = java.lang.Long.valueOf(runtimeService.getVariable(t.getProcessInstanceId, "ApplicationId").toString)
    val applicant = runtimeService.getVariable(t.getProcessInstanceId, "Applicant").toString

    val ss = Int.box(1)
    //val i: Object = ss
    /* Add comments to the Task for that Process Instance */
    val user = userId.userId
    val st = s"WIP"
    taskService.addComment(t.getId, t.getProcessInstanceId, comment)
    //taskService.setVariableLocal(t.getId(), "isEligible", 1 )
    taskService.setVariableLocal(t.getId(), "comment", comment) //TODO:- need to delete it if the process vaiable is not sed else where
    //taskService.complete(t.getId(), Map("isEligible" -> ss ), false)

    /** Update Application DB - Update Application Status **/
    val stsc = "WIP"
    val s = s"$stsc (by $user)"
    updateAppStatus(ApplicationId(applicationId), st ) //TODO:- should we update Application status to show to user in BEIS forms

    Future.successful(Option(LocalTaskId(t.getId)))
  }



  override def submitProcess(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]] = {

    val processEngine: ProcessEngine = ActivitiTaskService.apply()
    val runtimeService:RuntimeService = processEngine.getRuntimeService()
    val taskService:TaskService = processEngine.getTaskService()

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

  override def updateAppStatus(id: ApplicationId, appStatus: String): Future[Option[ApplicationId]] ={
    postWithResult[ApplicationId, String](urls.appStatus(id), appStatus)
  }

  override def saveMessageBoard(message: Message): Future[Option[MessageId]] ={
    postWithResult[MessageId, Message](urls.addMessage(), message)
  }

  override def updateMessageBoard(id: ApplicationId, message: String): Future[Option[MessageId]] ={
    postWithResult[MessageId, String](urls.updateMessage(id), message)
  }
}

object ActivitiTaskService{

  def apply(): ProcessEngine = {
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

    val processEngine: ProcessEngine = prconfig.buildProcessEngine()
    processEngine
  }

}

