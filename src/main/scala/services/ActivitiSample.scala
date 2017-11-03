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

import org.activiti.engine.history.HistoricProcessInstanceQuery
import org.activiti.engine.history.HistoricTaskInstance
import org.activiti.engine.{ProcessEngine, ProcessEngineConfiguration, TaskService}
import org.activiti.engine.impl.cfg.StandaloneProcessEngineConfiguration
import org.activiti.engine.task.Task
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils

/**
  * Created by venkatamutyala on 06/06/2017.
  *
  * Test purpose file - DELETE IT
  */
object ActivitiSample {

//TODO:- DELETE THIS FILE

  def main(args: Array[String]): Unit = {
    val jdbcUrl = "jdbc:postgresql://localhost:5432/activiti"

    val prconfig :ProcessEngineConfiguration = new StandaloneProcessEngineConfiguration()
      .setJdbcUrl(jdbcUrl)
      .setJdbcUsername("activiti")
      .setJdbcPassword("activiti")
      .setJdbcDriver("org.postgresql.Driver")
      .setDatabaseSchemaUpdate(ProcessEngineConfiguration.DB_SCHEMA_UPDATE_TRUE)

    val processEngine: ProcessEngine = prconfig.buildProcessEngine()
    val procInst = "237606"
    import scala.collection.JavaConversions._

    val taskService:TaskService = processEngine.getTaskService()
    //val historyQuery:HistoricProcessInstanceQuery = processEngine.getHistoryService().createHistoricProcessInstanceQuery().processDefinitionId("process2:3:157504").

    //      processEngine.getHistoryService().createHistoricProcessInstanceQuery()
    //      .finished()
    //      .processDefinitionId("XXX")
    //      .orderByProcessInstanceDuration().desc()
    //      .listPage(0, 10)


    val histPrc = processEngine.getHistoryService().createHistoricVariableInstanceQuery()
      .processInstanceId(procInst).orderByVariableName.desc().list()

    val histPrc1 = processEngine.getHistoryService().createHistoricProcessInstanceQuery()
      .processInstanceId(procInst)
   //histPrc.map(a => a.)
   // val histPrc1 = processEngine.getRuntimeService().createProcessInstanceQuery().processInstanceId("73568")
   //val tasks = taskService.createTaskQuery().taskCandidateGroup("accountancy").list()



    val taskInstanceList: Seq[HistoricTaskInstance] = processEngine.getHistoryService()
        .createHistoricTaskInstanceQuery()
      .processInstanceId(procInst).orderByTaskId().asc()
       .list()

    taskInstanceList.map{ tk =>
      //println("AAAA Task:-----"+ tk.getName + "---"+ tk.getAssignee + "---"+ tk.getId+ "---" + tk.getStartTime+ "---" + tk.getEndTime)
      println("------------------------------")
        println( tk.getName + "---"+ tk.getId+ tk.getAssignee+ "---" + tk.getStartTime+ "---" + tk.getEndTime)
     // println("*********:-----"+StringUtils.isEmpty(processEngine.getTaskService.getTaskComments(tk.getId).head.getFullMessage))
     //     if(!StringUtils.isEmpty(processEngine.getTaskService.getTaskComments(tk.getId).head.getFullMessage)) {
     //        println("Comments:-" + processEngine.getTaskService.getTaskComments(tk.getId).head.getFullMessage)
     //      }
      println("000*********:-----"+ tk.getProcessVariables.get("Applicant"))
      println("000*********:-----"+ tk.getProcessVariables.get("approvestatus"))

      println("111*********:-----"+processEngine.getHistoryService().createHistoricDetailQuery().variableUpdates().taskId(tk.getId()).list().size())

      if(processEngine.getHistoryService().createHistoricVariableInstanceQuery().taskId(tk.getId).variableName("Applicant").singleResult()!= null) {
        println("2a2a*********:-----" + processEngine.getHistoryService().createHistoricVariableInstanceQuery().taskId(tk.getId)
          .variableName("Applicant").singleResult().getValue())
      }

      if(processEngine.getHistoryService().createHistoricVariableInstanceQuery().taskId(tk.getId).variableName("approvestatus").singleResult()!= null) {
        println("2b2b2*********:-----" + processEngine.getHistoryService().createHistoricVariableInstanceQuery().taskId(tk.getId)
          .variableName("approvestatus").singleResult().getValue())
      }
            println("2c2c2*********:-----"+ processEngine.getHistoryService().createHistoricVariableInstanceQuery().processInstanceId(procInst)
        .variableName("Applicant").singleResult().getValue())

      //println("333*********:-----"+ processEngine.getHistoryService().createHistoricVariableInstanceQuery().taskId(tk.getId)
      //  .variableName("approvestatus").singleResult().getValue())

      /*if(processEngine.getTaskService.getTaskComments(tk.getId).size() > 0) {
        println("Comments:-" + processEngine.getTaskService.getTaskComments(tk.getId).head.getFullMessage)
       // println("Status:-" + processEngine.getTaskService.getVariable(tk.getId, "approvestatus"))
       /// println("Status:-" +
         // processEngine.getHistoryService().createHistoricDetailQuery().variableUpdates().taskId(tk.getId()).list().size())
        processEngine.getHistoryService().createHistoricDetailQuery().variableUpdates().processInstanceId(procInst).list().map( s =>
          println("Comments:-" + s.getId
        ))
      }*/
    }

    // processEngine.getTaskService.getProcessInstanceComments("73505").map { a=>
   //   println("*********"+ a.getFullMessage)
    // }
    //-----------------------
    //    val tasks = taskService.createTaskQuery().taskAssignee("approver1").list()
    //    tasks.map{
    //      task=> println("Task:-----"+ task.getName + "---"+ task.getId+ "---"+ task.getFormKey)
    //      taskService.getVariables(task.getId()).map(
    //        s=> println("=========map:-" + s._1 +"--"+ s._2)
    //      )
    //    }

    println("========map:-" +taskService.createTaskQuery().processInstanceId(procInst).list().size())

  }

}
