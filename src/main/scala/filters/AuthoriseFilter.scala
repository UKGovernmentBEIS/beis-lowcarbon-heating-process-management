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

package filters

import javax.inject.Inject

import akka.stream.Materializer
import config.Config
import org.apache.commons.lang3.StringUtils
import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Breaks._
import scala.util.control._

/**
  * Created by venkatamutyala on 06/02/2018.
  */

class AuthoriseFilter @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter {

  import play.api.mvc.Results._
  implicit val messages = Messages

  override def apply(nextCall: (RequestHeader) => Future[Result])( rh: RequestHeader): Future[Result] = {

    if (rh.uri.startsWith("/assets") || rh.uri.startsWith("/resetpassword")|| rh.uri.startsWith("/opportunity")
      || isSessionExcluded(rh) /* || rh.uri.startsWith("/pdf")*/) {
      nextCall(rh)
    }
    else {

        isSessionTimedOut(rh.session.get("sessionTime").getOrElse(System.currentTimeMillis.toString).toLong) match {
          case true =>
            val errMsg = Messages("error.BF002")
            Future.successful(Ok(views.html.loginForm(errMsg, None)))

          case false => {
            rh.session.get ("username_process").map {
              user =>
                nextCall (rh).flatMap {
                  a =>
                    Future (a.withSession (
                      ("username_process" -> rh.session.get ("username_process").get),
                      ("role" -> rh.session.get ("role").get),
                      ("sessionTime" -> System.currentTimeMillis.toString)
                    ))
                }
            }.getOrElse {
              if(StringUtils.isNotEmpty(rh.getQueryString("token").getOrElse(""))) {
                nextCall (rh)
              }else
              Future.successful (Ok (views.html.loginForm (Messages("error.BF002")) ) )
            }
          }
        }
    }
  }

  def isSessionTimedOut(sessionTime: Long):Boolean = {
    val sessionTimeout = Config.config.login.sessionTimeout
    val currentTime = System.currentTimeMillis
    (currentTime - sessionTime) > sessionTimeout
  }

  def isSessionExcluded(rh: RequestHeader):Boolean = {
    val Outer = new Breaks
    Outer.breakable {
      Config.config.login.excludeSession.split(",").map { endpoint =>
        if (rh.path.equals(endpoint)) {
          return true
          break
        }
      }
    }
    false
  }
}