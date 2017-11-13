package services

/**
  * Created by venkatamutyala on 13/06/2017.
  */
sealed trait Status {
   def status:String
}

case object Approved extends Status {
  override val status = "Approved"
}
case object NotApproved extends Status {
  override val status = "Not Approved"
}

case object Reviewed extends Status {
  override val status = "Reviewed"
}

case object Rejected extends Status {
  override val status = "Rejected"
}

case object NeedMoreInfo extends Status {
  override val status = "needmoreinfo"
}

case object InProgress extends Status {
  override val status = "Inprogress"
}

case object Complete extends Status {
  override val status = "Complete"
}

case object Eligible extends Status {
  override val status = "Eligible"
}

case object NotEligible extends Status {
  override val status = "Not eligible"
}

case object AssessorsAssigned extends Status {
  override val status = "Assessors assigned"
}

case object Assessed extends Status {
  override val status = "Assessed"
}

case object Moderated extends Status {
  override val status = "Score Moderated"
}