package models

import org.joda.time.DateTime

/**
  * Created by venkatamutyala on 12/06/2017.
  */
case class Message(id: MessageId, userId: UserId, applicationId: ApplicationId, sectionNumber: Int, sentBy: UserId, sentAt: DateTime, message : Option[String])

case class MessageId(id:Long)
