package models

/**
  * Created by venkatamutyala on 26/06/2017.
  */

case class ProcessDefinition (processDefinitionId: ProcessDefinitionId, businessKey: BusinessKey, returnVariables: Boolean,
                              variables: Seq[ProcessVariable])
case class ProcessInstance (id: String, url:String, businessKey: BusinessKey, suspended: Boolean, ended: Boolean, processDefinitionId:String,
                            processDefinitionUrl:String, processDefinitionKey:String, activityId: String, variables: Seq[String], tenantId: String,
                            name: String, completed: Boolean)

case class BusinessKey(id: String)
case class ProcessId(id: LongId)
case class ProcessDefinitionId(id: String)
case class ProcessInstanceId(id: String)
case class ProcessVariable(name: String,  scope: String, vartype: String, value: String)
case class ProcessVariableMap(variableMap: Map[String, String] )
case class ActionId(id: String)
case class Comment(message: String)
