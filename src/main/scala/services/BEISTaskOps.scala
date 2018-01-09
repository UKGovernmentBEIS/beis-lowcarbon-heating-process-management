package services

import com.google.inject.ImplementedBy
import models._

import scala.concurrent.Future

/**
  * Created by venkatamutyala on 07/06/2017.
  */

@ImplementedBy(classOf[BEISTaskService])
trait BEISTaskOps {

  def showTask(id: LocalTaskId): Future[Option[LocalTask]]
  def showTasks(userId: UserId): Future[Seq[LocalTaskSummary]]
  def submitProcess(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]]
  def updateAppStatus(id: ApplicationId, status: String): Future[Option[ApplicationId]]
  def saveMessageBoard(message: Message): Future[Option[MessageId]]
  def updateMessageBoard(id: ApplicationId, message: String): Future[Option[MessageId]]
  def submitEligibility(id: LocalTaskId, userId: UserId, status: String, comment: String, technology: String, processInstanceId: String): Future[Option[LocalTaskId]]
  def submitAssignAssessors(id: LocalTaskId, userId: UserId, assignassessor1: String, assignassessor2: String,
                            assignassessor3: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]]
  def submitAssessment(id: LocalTaskId, userId: UserId, asmtKey: Int,  score:Score,
                       processInstanceId: String, buttonAction: String): Future[Option[LocalTaskId]]
  def submitMakePanelDecision(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]]

  def submitModerateScore(id: LocalTaskId, userId: UserId, status: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]]
  def submitConfirmEmailSent(id: LocalTaskId, userId: UserId, emailsent: String, comment: String, processInstanceId: String): Future[Option[LocalTaskId]]

  def submitLogin(userId: String, password: String): Future[Option[String]]
  def updatePassword(userId: String, password: String, newpassword: String): Future[Int]
  def saveUser(userId: String, firstName: String, lastName: String, password: String, email: String): Future[Int]

  def getMembers(groupIds: Seq[String]): Option[Map[String,String]]

}