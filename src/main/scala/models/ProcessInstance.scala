package models

/**
  * Created by venkatamutyala on 26/06/2017.
  */

case class ProcessDefinition (processDefinitionId: ProcessDefinitionId, businessKey: BusinessKey, returnVariables: Boolean,
                              variables: Seq[ProcessVariable])

/*case class ProcessInstance (id: String, url:String, businessKey: BusinessKey, suspended: Boolean, ended: Boolean, processDefinitionId:String,
                            processDefinitionUrl:String, processDefinitionKey:String, activityId: String, variables: Seq[String], tenantId: String,
                            name: String, completed: Boolean)*/

/* This class is mainly used for Excel report creation. So all variables are Strings */
case class LocalProcessInstance(processInstanceId:String, name: String, applicant: String, status: String, appId: Long, appRef: String,
                                  organisation: String, projectTitle: String, projectValue: String, grantValue: String,
                                  technology: String, maxDeviation: Double, averageweightedscore: Double, averagetiebreakscore: Double, averagemoderatescore: Double,
                                assessor1: String, assessor2: String, assessor3: String,
                                assessor1score: String, assessor2score: String, assessor3score: String
                               )

case class ProcessInstanceSummary(processInstanceId:String, name: String, applicant: String, status: String, appId: Long, appRef: String,
                                  technology: String, averageweightedscore: Double, averagetiebreakscore: Double)
case class BusinessKey(id: String)
case class ProcessId(id: Long)
case class ProcessDefinitionId(id: String)
case class ProcessInstanceId(id: String)
case class ProcessVariable(name: String,  scope: String, vartype: String, value: String)
case class ProcessVariableMap(variableMap: Map[String, String] )
case class ActionId(id: String)
case class Comment(message: String)
case class LocalProcess(piId:ProcessId, appId: Long, appRef: String, oppId: Long, oppTitle: String, status: String,
                        taskHistories:Seq[TaskHistory], additionalInfo: Option[String] = None)
