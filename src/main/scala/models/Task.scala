package models


/**
  * Created by venkatamutyala on 07/06/2017.
  */

//
case class LocalTask(id: LocalTaskId, key: String, name: String, groups: List[String], applicant: UserId, status: String, appId: Long,
                     appRef: String, oppId: Long, oppTitle: String, description: String, processDefinitionId: ProcessDefinitionId,
                     processInstanceId: ProcessInstanceId, taskHistories:Seq[TaskHistory], variables: Map[String, AnyRef])

case class LocalTaskSummary(id: LocalTaskId, key: String,  name: String, assignedTo: String ,applicant: UserId, status: String, appId: Long, appRef: String, oppId: Long,
                            technology: String, averageweightedscore: Double, averagetiebreakscore: Double)
case class LocalTaskId(id: String)
case class LocalComment(time: String, fullMessage: String)
case class TaskHistory(name: String, assignee: String, startTime: String, endTime: Option[String],
                       status: String, fullMessages: Seq[LocalComment])
case class TaskAction(action: ActionId, variables: Seq[ProcessVariable])
