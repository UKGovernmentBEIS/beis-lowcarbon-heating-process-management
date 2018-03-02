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

package forms.services

import com.google.inject.Inject
import models.{AppAuthPayload, AppSectionNumber, ApplicationId}
import org.scalatest.{Matchers, WordSpecLike}
import play.api.Play
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test
import play.api.libs.json.Json
import services._

class JWTTokenTest  extends WordSpecLike with Matchers{

  implicit val appAuthPayloadWrites = Json.writes[AppAuthPayload]

  val urls = new ApplicationURLs("")

  val payload = Json.toJson(AppAuthPayload("policyadmin", "tester", "1", 36000)).toString()

  val app: play.api.Application = new GuiceApplicationBuilder()
    .configure("some.configuration" -> "value")
    .build()

  val jwt = app.injector.instanceOf[JWTOps]

  "JWTToken " should {

    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9saWN5YWRtaW4iLCJ1c2VyIjoidGVzdGVyIiwiYXBwaWQiOiIxIiwiZXhwIjozNjAwMH0.ragOgM-Ru-cfAIjQkcIo870XlOLZrAgWAaOBbI1Li9Q"

    "JWT Auth Token created " in {
      jwt.createToken(payload) shouldBe token
    }
  }

  "JWTToken created " should {

    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9saWN5YWRtaW4iLCJ1c2VyIjoidGVzdGVyIiwiYXBwaWQiOiIxIiwiZXhwIjozNjAwMH0.ragOgM-Ru-cfAIjQkcIo870XlOLZrAgWAaOBbI1Li9Q"

    "valid when validate " in {
      jwt.isValidToken(token) shouldBe true
    }

    val token_notvalid = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlI___joicG9saWN5YWRtaW4iLCJ1c2VyIjoidGVzdGVyIiwiYXBwaWQiOiIxIiwiZXhwIjozNjAwMH0.ragOgM-Ru-cfAIjQkcIo870XlOLZrAgWAaOBbI1Li9Q"

    "not valid when validate " in {
      jwt.isValidToken(token_notvalid) shouldBe false
    }

  }

  "JWTToken created with a Payload" should {

    val token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoicG9saWN5YWRtaW4iLCJ1c2VyIjoidGVzdGVyIiwiYXBwaWQiOiIxIiwiZXhwIjozNjAwMH0.ragOgM-Ru-cfAIjQkcIo870XlOLZrAgWAaOBbI1Li9Q"
    val payload = Json.toJson(AppAuthPayload("policyadmin", "tester", "1", 36000)).toString()

    "Payload is validated as " in {
      jwt.decodePayload(token).getOrElse("") shouldBe payload
    }
  }

}
