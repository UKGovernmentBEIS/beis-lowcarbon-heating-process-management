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

package controllers

import java.io.{BufferedOutputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}
import java.net.URL
import java.util.zip.ZipOutputStream
import javax.inject.Inject

import org.joda.time.DateTime
import org.w3c.dom.Document
import org.w3c.tidy.Tidy
import akka.stream.scaladsl.StreamConverters
import models.{AppAuthPayload, ApplicationId, UserId}
import org.w3c.dom.Document
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import services.{BEISTaskOps}

import scala.concurrent.{ExecutionContext, Future}


class ExportDataController  @Inject()(localtasks: BEISTaskOps )(implicit ec: ExecutionContext) extends Controller {


  def exportDataToExcel = Action.async { implicit request =>

    val linebreaker = "\n"
    val userId = request.session.get("username_process").getOrElse("Unauthorised User")
    var baos = new ByteArrayOutputStream()
    val str = s"Application, Status, Organisation, Project Title , Technology, Project Value , Grant Value , Assessor1, Assessor2, Assessor3, " +
      s"Assessor1 Score, Assessor2 Score, Assessor3 Score, Max Score Range, Weighted score, Tie-break score, Moderated Score"
    baos.write(str.getBytes)
    baos.write(linebreaker.getBytes)

    for(
      ps <- localtasks.showProcesses(UserId(userId))
    )yield {
      ps.map { p =>
        val vl = s"${p.appId + 1000} - ${p.appRef}, ${p.status}, ${p.organisation}, ${p.projectTitle}, ${p.technology}, ${p.projectValue}, ${p.grantValue}, " +
          s"${p.assessor1},  ${p.assessor2}, ${p.assessor3}, ${p.assessor1score},  ${p.assessor2score}, ${p.assessor3score},  ${p.maxDeviation}, " +
          s"${p.averageweightedscore}, ${p.averagetiebreakscore}, ${p.averagemoderatescore} "
        baos.write(vl.getBytes)
        baos.write(linebreaker.getBytes)
      }

      baos.close()

      val timestamp = new DateTime().getMillis
      val inp: ByteArrayInputStream = new ByteArrayInputStream(baos.toByteArray)
      inp.close()
      Ok.chunked(StreamConverters.fromInputStream(() => inp)).withHeaders(
        //CONTENT_TYPE -> "application/vnd.ms-excel",
        CONTENT_TYPE -> "text/csv",
        CONTENT_DISPOSITION -> s"attachment; filename = ProcessListData_$timestamp.csv"
      )
    }
  }

}
