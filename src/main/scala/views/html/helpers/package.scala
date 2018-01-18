package views.html

import javax.inject.Inject

import config.Config
import models.AppAuthPayload
import org.joda.time.DateTime
import play.api.Play
import play.api.libs.json.Json
import services.{BEISTaskOps, JWTOps}

/**
  * Created by venkatamutyala on 15/01/2018.
  */


package object helpers {

  private def instance = Play.current.injector.instanceOf[JWT]
  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  def JWTToken(grpId: String, userId: String, appId: String) = {

    val exp = Config.config.jwt.exp
    val dateTime = new DateTime()
    val expiry = new DateTime((dateTime.getMillis + exp.toLong)).getMillis
    instance.getJWTToken(grpId, userId, appId, expiry)
  }
}

/**  ::: JWT:::
  * The JWT Token created here in Package in an object, can be used directly in any HTML page or controller.
  * eg: fileUpload.html (to download a AWS file) etc
  **/

case class JWT  @Inject()(jwt: JWTOps ) {
  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  def getJWTToken(grpId: String, userId: String, appId: String, exp: Long) = {
    val appAuthpayload =  Json.toJson(AppAuthPayload(grpId, userId, appId.toString, exp)).toString()
    val appAuthToken = jwt.createToken(appAuthpayload)
    val appFrontEndUrlWithJWTToken = s"$appAuthToken"
    appFrontEndUrlWithJWTToken
  }
}

