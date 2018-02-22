package models

import play.api.libs.json.JsObject

/**
  * Created by venkatamutyala on 12/06/2017.
  */
case class Application(id:ApplicationId)
case class ApplicationId(id:Long)
case class AppSectionNumber(num: Int)


case class ApplicationSection(applicationId: Long, sectionNumber: Long, answers: JsObject)