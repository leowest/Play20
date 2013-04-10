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

  val userMapWithInfo =
    userMap ++ Map(
      "informations.label" -> Seq("Personal"),
      "informations.email" -> Seq("fakecontact@gmail.com"),
      "informations.phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

  val userJson = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "Tournay",
    "age" -> 27)

  import play.api.data.validation2._
  import Extractors._
  import Constraints._

  val name = (Path \ "firstname").validate(nonEmptyText)

  "Map Validation" should {
    "extract data" in {
      (Path \ "firstname").validate[Seq[String]](userMap) mustEqual(Success(Seq("Julien")))
    }

    "validate data" in {
      name(userMap) mustEqual(Success("Julien"))
    }

    "validate deep" in {
      val v = (Path \ "informations").validate(
                (Path \ "label").validate[String])

      v(userMapWithInfo) mustEqual(Success("Personal"))
    }
  }

  "Json Validation" should {
    "extract data" in {
      (Path \ "firstname").validate[String](userJson) mustEqual(Success("Julien"))
    }

    "validate data" in {
      name(userJson) mustEqual(Success("Julien"))
    }
  }
}