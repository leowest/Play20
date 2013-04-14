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

  type M = Map[String, Seq[String]]
  type J = JsValue

  "Map / Json Validation" should {

    "extract data" in {

      (Path \ "firstname").validate[M, Seq[String]].validate(userMap) mustEqual(Success(Seq("Julien")))
      (Path \ "firstname").validate[J, String].validate(userJson) mustEqual(Success("Julien"))

      val errPath = Path \ "foobar"
      val error = Failure(Seq(errPath -> Seq("validation.required")))
      errPath.validate[M, String].validate(userMap)  mustEqual(error)
      errPath.validate[J, String].validate(userJson) mustEqual(error)
    }

    "validate data" in {

      (Path \ "firstname").validate[M, String](nonEmptyText).validate(userMap) mustEqual(Success("Julien"))
      (Path \ "firstname").validate[J, String](nonEmptyText).validate(userJson) mustEqual(Success("Julien"))

      (Path \ "lastname").validate[M, String](nonEmptyText).validate(userMap)  mustEqual(Failure(Seq((Path \ "lastname") -> Seq("validation.nonemptytext"))))
      (Path \ "lastname").validate[J, String](nonEmptyText).validate(userJson) mustEqual(Failure(Seq((Path \ "lastname") -> Seq("validation.nonemptytext"))))
    }

    "validate deep" in {
      (Path \ "informations").validate[M, String](
        (Path \ "label").validate[M, String]).validate(userMap) mustEqual(Success("Personal"))

      (Path \ "informations").validate[J, String](
        (Path \ "label").validate[J, String]).validate(userJson) mustEqual(Success("Personal"))
    }

    "coerce type" in {

      (Path \ "age").validate[M, Int].validate(userMap) mustEqual(Success(27))
      (Path \ "age").validate[J, Int].validate(userJson) mustEqual(Success(27))

      (Path \ "firstname").validate[M, Int].validate(userMap) mustEqual(Failure(Seq((Path \ "firstname") -> Seq("validation.int"))))
      (Path \ "firstname").validate[J, Int].validate(userJson) mustEqual(Failure(Seq((Path \ "firstname") -> Seq("validation.int"))))
    }

    "compose constraints" in {
      // TODO: create MonoidOps
      import Validations._
      val composed = monoidConstraint.append(nonEmptyText, minLength(3))

      (Path \ "firstname").validate[M, String](composed).validate(userMap) mustEqual(Success("Julien"))
      (Path \ "firstname").validate[J, String](composed).validate(userJson) mustEqual(Success("Julien"))

      val err = Failure(Seq((Path \ "lastname") -> Seq("validation.nonemptytext", "validation.minLength")))

      (Path \ "lastname").validate[M, String](composed).validate(userMap) mustEqual(err)
      (Path \ "lastname").validate[J, String](composed).validate(userJson) mustEqual(err)
    }

    "compose validations" in {
      import play.api.libs.functional.syntax._
      import Validations._

      val x = (Path \ "firstname").validate[M, String](nonEmptyText)
      val y = (Path \ "lastname").validate[M, String](nonEmptyText)

      val userFromMap = (x ~ y) { _ -> _ }

      println(userFromMap.validate(userMap))

      success
    }

  }

}