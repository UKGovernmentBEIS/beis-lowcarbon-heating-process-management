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

import java.util.Base64

import config.Config
import play.api.Logger
import play.api.data.validation.ValidationError
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import services.RestService.{JsonParseException, RestFailure}

import scala.concurrent.{ExecutionContext, Future}

trait RestService {

  def ws: WSClient

  implicit def ec: ExecutionContext

  private val authorizationHeader = "Authorization"
  private val basicAuth = {
    "Basic " + new String(Base64.getEncoder.encode((Config.config.bpmrest.procuser + ":" + Config.config.bpmrest.procpwd).getBytes))
  }

  def getOpt[A: Reads](url: String): Future[Option[A]] = {
    val request: WSRequest = ws.url(url)
    request.get.map { response =>
      response.status match {
        case 200 => {
            response.json.validate[A] match {
            case JsSuccess(a, _) => Some(a)
            case JsError(errs) => {
               throw JsonParseException("GET", request, response, errs)
            }
          }
        }
        case 404 => None
        case _ => throw RestFailure("GET", request, response)
      }
    }
  }

  def getMany[A: Reads](url: String): Future[Seq[A]] = {
    val request: WSRequest = ws.url(url)
    request.get.map { response =>
      response.status match {
        case 200 => response.json.validate[Seq[A]] match {
          case JsSuccess(as, _) => as
          case JsError(errs) => throw JsonParseException("GET", request, response, errs)
        }
        case _ => throw RestFailure("GET", request, response)
      }
    }
  }

  def post[A: Writes](url: String, body: A): Future[Unit] = {
    val request = ws.url(url)
    request.post(Json.toJson(body)).map(_ => ())
  }

  def postFile[A: Writes](url: String, body: A): Future[Int] = {
    val request = ws.url(url)
    request.post(Json.toJson(body)).map{ response =>
      response.status match {
        case 200 => response.json.validate[Int] match {
          case JsSuccess(x,_) => x
          case JsError(errors) => throw JsonParseException("POST", request, response, errors)
        }
        case _ => throw RestFailure("POST", request, response)
      }
    }
  }

  def put[A: Writes](url: String, body: A): Future[Unit] = {
    val request = ws.url(url)
    request.put(Json.toJson(body)).map(_ => ())
  }

  def delete(url: String): Future[Unit] = {
    val request = ws.url(url)
    request.delete().map(_ => ())
  }

  def postWithResult[A: Reads, B: Writes](url: String, body: B): Future[Option[A]] = {
    val request:WSRequest = ws.url(url)
    request.post(Json.toJson(body)).map { response =>
      response.status match {
        case 200 => response.json.validate[A] match {
          case JsSuccess(a, _) =>  Some(a)
          case JsError(errs) => throw JsonParseException("POST", request, response, errs)
        }
        case 404 => None
        case _ => throw RestFailure("POST", request, response)
      }
    }
  }

  def getWithHeaderUpdate[A: Reads, B: Writes](url: String, body: B): Future[Option[A]] = {
    val request:WSRequest = ws.url(url)
    val requestWithUserHeader:WSRequest  = request.withHeaders("userId" -> body.toString)
    requestWithUserHeader.get.map { response =>
      response.status match {
        case 200 => response.json.validate[A] match {
          case JsSuccess(a, _) =>  Some(a)
          case JsError(errs) => throw JsonParseException("GET", request, response, errs)
        }
        case 404 => None
        case _ => throw RestFailure("GET", request, response)
      }
    }
  }

  def getManyOptWithAuthHeaderUpdate[A: Reads, B: Writes](url: String,  auth: String, body: B): Future[A] = {
    val request:WSRequest = ws.url(url)
    val requestWithUserHeader: WSRequest = request.withHeaders((authorizationHeader, auth))
      .withHeaders("userId" -> body.toString)

    requestWithUserHeader.get.map { response =>

      response.status match {
        case 200 => (response.json \ "data" \ "id").validate[A] match {
          case JsSuccess(as, _) => {
            as
          }
          case JsError(errs) => {
            throw JsonParseException("GET", request, response, errs)
          }
        }
        case _ => throw RestFailure("GET", request, response)
      }
    }
  }

  def getOptWithAuthHeaderUpdate[A: Reads](url: String,  auth: String): Future[Option[A]] = {
    val request:WSRequest = ws.url(url)
    val requestWithUserHeader: WSRequest = ws.url(url).withHeaders((authorizationHeader, auth))
    requestWithUserHeader.get.map { response =>

      response.status match {
        case 200 => {
          val d = response.json \ "data"
          (d (0) \ "id").validate[A] match {
            case JsSuccess(a, _) => Some(a)
            case JsError(errs) => throw JsonParseException("GET", request, response, errs)
          }
        }
        case 404 => None
        case _ => throw RestFailure("GET", request, response)
      }
    }
  }

  def postOptWithAuthHeaderUpdate[A: Reads, B: Writes](url: String,  auth: String, body: B): Future[Option[A]] = {
    val request:WSRequest = ws.url(url)
    val requestWithUserHeader: WSRequest = ws.url(url).withHeaders((authorizationHeader, auth))
    requestWithUserHeader.post(Json.toJson(body)).map { response =>
      response.status match {
        case 200 => response.json.validate[A] match {
          case JsSuccess(a, _) =>  Some(a)
          case JsError(errs) => throw JsonParseException("GET", request, response, errs)
        }
        case 404 => None
        case _ => throw RestFailure("GET", request, response)
      }
    }
  }

}

object RestService {

  case class JsonParseException(method: String, request: WSRequest, response: WSResponse, errs: Seq[(JsPath, Seq[ValidationError])]) extends Exception

  case class RestFailure(method: String, request: WSRequest, response: WSResponse) extends Exception {
    val status = response.status
  }

}
