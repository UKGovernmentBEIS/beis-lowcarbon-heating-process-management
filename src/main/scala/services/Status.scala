package services

/**
  * Created by venkatamutyala on 13/06/2017.
  */
sealed trait Status {
   def status:String
}

case object Approved extends Status {
  override val status = "approved"
}

case object Reviewed extends Status {
  override val status = "reviewed"
}

case object Rejected extends Status {
  override val status = "rejected"
}

case object NeedMoreInfo extends Status {
  override val status = "needmoreinfo"
}