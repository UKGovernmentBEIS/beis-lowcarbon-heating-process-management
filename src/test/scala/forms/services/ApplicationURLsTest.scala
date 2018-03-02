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

import models.{AppSectionNumber, ApplicationId}
import org.scalatest.{Matchers, WordSpecLike}
import services._

class ApplicationURLsTest extends WordSpecLike with Matchers {
  val urls = new ApplicationURLs("")

  "ApplicationURLsTest" should {

    val id = ApplicationId(1L)
    val sectionNum = AppSectionNumber(2)

    "appstatus in updateAppStatus, the correct url" in {
      urls.appStatus(id) shouldBe "/application/1/appstatus"
    }

    "updateMessage correct url" in {
      urls.updateMessage(id) shouldBe "/message/1/messageboard"
    }

    "to get the application section data, the correct url" in {
      urls.applicationSectionData(id, sectionNum) shouldBe "/application/1/section/2"
    }

  }

}
