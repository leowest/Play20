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

  import play.api.data.validation2._
  import Extractors._
  import Constraints._

  val name: Extractor[String] = (Path \ "firstname").validate(notEmptyText)

  "Map Validation" should {
    "extract data" in {
      (Path \ "firstname").validate[Seq[String]](userMap) mustEqual(Success(Seq("Julien")))
    }

    "validate data" in {
      name(userMap) mustEqual(Success("Julien"))
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