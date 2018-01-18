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

import authentikat.jwt.{JsonWebToken, JwtClaimsSet, JwtHeader}
import com.google.inject.Inject
import config.Config

import scala.concurrent.ExecutionContext


/**
  * Created by venkatamutyala on 04/01/2018.
  */
class JWTService  @Inject()(implicit val ec: ExecutionContext)
  extends JWTOps {

  val filedownloaddirectory = Config.config.jwt.jwtSecretKey

  val jwtSecretKey =  Config.config.jwt.jwtSecretKey
  val jwtSecretAlgo = Config.config.jwt.jwtSecretAlgo

  def createToken(payload: String): String = {
    val header = JwtHeader(jwtSecretAlgo)
    val claimsSet = JwtClaimsSet(payload)
    JsonWebToken(header, claimsSet, jwtSecretKey)
  }

  def isValidToken(jwtToken: String): Boolean =
    JsonWebToken.validate(jwtToken, jwtSecretKey)

  def decodePayload(jwtToken: String): Option[String] =
    jwtToken match {
      case JsonWebToken(header, claimsSet, signature) => Option(claimsSet.asJsonString)
      case _ => None
    }
}
