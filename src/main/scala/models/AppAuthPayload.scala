package models

/**
  * Created by venkatamutyala on 08/01/2018.
  */

/* This is the payload sent in the JWT tokem as Claims - Payload
   to Access Applcation Information*/
case class AppAuthPayload(role: String, user: String, appid: String)
