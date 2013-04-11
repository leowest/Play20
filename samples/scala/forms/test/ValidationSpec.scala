package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

class ValidationSpec extends Specification {

  val userMap = Map(
    "firstname" -> Seq("Julien"),
    "lastname" -> Seq(""),
    "age" -> Seq("27"),
    "informations.label" -> Seq("Personal"),
    "informations.email" -> Seq("fakecontact@gmail.com"),
    "informations.phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

  val userJson = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "",
    "age" -> 27,
    "informations" -> Json.obj(
      "label" -> "Personal",
      "email" -> "fakecontact@gmail.com",
      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10")))

  import play.api.data.validation2._
  import Extractors._
  import Constraints._

  "Map / Json Validation" should {
    "extract data" in {
      (Path \ "firstname").validate[Seq[String]](userMap) mustEqual(Success(Seq("Julien")))
      (Path \ "firstname").validate[String](userJson) mustEqual(Success("Julien"))

      val errPath = Path \ "foobar"
      val error = Failure(Seq(errPath -> Seq("validation.required")))
      errPath.validate[String](userMap)  mustEqual(error)
      errPath.validate[String](userJson) mustEqual(error)
    }

    "validate data" in {
      val firstname = (Path \ "firstname").validate(nonEmptyText)
      val lastname = (Path \ "lastname").validate(nonEmptyText)

      firstname(userMap) mustEqual(Success("Julien"))
      firstname(userJson) mustEqual(Success("Julien"))

      lastname(userMap)  mustEqual(Failure(Seq((Path \ "lastname") -> Seq("validation.nonemptytext"))))
      lastname(userJson) mustEqual(Failure(Seq((Path \ "lastname") -> Seq("validation.nonemptytext"))))
    }

    "validate deep" in {
      val v =
        (Path \ "informations").validate(
          (Path \ "label").validate[String])

      v(userMap) mustEqual(Success("Personal"))
      v(userJson) mustEqual(Success("Personal"))
    }

    "coerce type" in {
      val age = (Path \ "age").validate[Int]

      age(userMap) mustEqual(Success(27))
      age(userJson) mustEqual(Success(27))

      (Path \ "firstname").validate[Int](userMap) mustEqual(Failure(Seq((Path \ "firstname") -> Seq("validation.int"))))
      (Path \ "firstname").validate[Int](userJson) mustEqual(Failure(Seq((Path \ "firstname") -> Seq("validation.int"))))
    }

    "compose constraints" in {
      import syntax._
      // TODO: create MonoidOps
      val composed = monoidConstraint.append(nonEmptyText, minLength(3))

      val firstname = (Path \ "firstname").validate(composed)
      firstname(userMap) mustEqual(Success("Julien"))
      firstname(userJson) mustEqual(Success("Julien"))

      val err = Failure(Seq((Path \ "lastname") -> Seq("validation.nonemptytext", "validation.minLength")))

      val lastname = (Path \ "lastname").validate(composed)
      lastname(userMap) mustEqual(err)
      lastname(userJson) mustEqual(err)
    }

    "compose validations" in {
      import syntax._
      import play.api.libs.functional.syntax._

      //val user = (Path \ "firstname").validate(nonEmptyText) ~
      //           (Path \ "lastname").validate(nonEmptyText) ~
      //           (Path \ "age").validate[Int]
      success
    }
  }

}