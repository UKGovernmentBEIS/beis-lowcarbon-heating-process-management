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

package config

case class BusinessConfig(appFrontEndUrl: String, appBackEndUrl: String, bpmServerUrl: String, emailto: String, addressSearch: String)
case class BPMConfig(url: String, driver: String, user: String, pwd: String, procdefId: String, procReview: String, procApprove: String,
                      policyadmingroup: String, assessorgroup: String)
case class BPMRestConfig(procuser: String, procpwd: String)
case class JWTConfig(jwtSecretKey: String, jwtSecretAlgo: String, appAccessRole: String)

case class Config(logAssets: Option[Boolean], logRequests: Boolean, business: BusinessConfig, bpm: BPMConfig, bpmrest: BPMRestConfig,
                  jwt: JWTConfig)


object Config {

  import pureconfig._

  lazy val config: Config = loadConfig[Config].get

}
