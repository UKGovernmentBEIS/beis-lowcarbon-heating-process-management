package views.html

import javax.inject.Inject

import config.Config
import models.AppAuthPayload
import play.api.Play
import play.api.libs.json.Json
import services.{BEISTaskOps, JWTOps}

/**
  * Created by venkatamutyala on 15/01/2018.
  */

case class JWT  @Inject()(jwt: JWTOps ) {
  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  def getJWTToken(grpId: String, userId: String, appId: String) = {
    val appFrontEndUrl = Config.config.business.appFrontEndUrl
    val appAuthpayload =  Json.toJson(AppAuthPayload(grpId, userId, appId.toString)).toString()
    val appAuthToken = jwt.createToken(appAuthpayload)
    val appFrontEndUrlWithJWTToken = s"$appFrontEndUrl/simplepreview/${appId}?token=$appAuthToken"
    appFrontEndUrlWithJWTToken
  }
}

package object helpers {
  private def instance = Play.current.injector.instanceOf[JWT]
  val appFrontEndUrl = Config.config.business.appFrontEndUrl
  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  def JWTToken(grpId: String, userId: String, appId: String) = {
    instance.getJWTToken(grpId, userId, appId)
  }
}
