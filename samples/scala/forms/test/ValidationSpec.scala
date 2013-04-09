package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

class ValidationSpec extends Specification {

  val userMap = Map(
    "firstname" -> Seq("Julien"),
    "lastname" -> Seq("Tournay"),
    "age" -> Seq("27"))

  val userJson = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "Tournay",
    "age" -> 27)

  import play.api.data.validation._
  import Mappings._

  "Validation" should {

    "extract data from a Map" in {
      (Path \ "firstname")(userMap) mustEqual(Success(Seq("Julien")))
    }

    "extract data from a JsValue" in {
      (Path \ "firstname")(userJson) mustEqual(Success(Seq(JsString("Julien"))))
    }

  }
}