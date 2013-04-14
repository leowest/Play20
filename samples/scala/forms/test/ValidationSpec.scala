package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

class ValidationSpec extends Specification {

  val validMap = Map(
    "firstname" -> Seq("Julien"),
    "lastname" -> Seq("Tournay"),
    "age" -> Seq("27"),
    "informations.label" -> Seq("Personal"),
    "informations.email" -> Seq("fakecontact@gmail.com"),
    "informations.phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

  val invalidMap = Map(
     "firstname" -> Seq("Julien"),
     "lastname" -> Seq("Tournay"),
     "age" -> Seq("27"),
     "informations.label" -> Seq(""),
     "informations.email" -> Seq("fakecontact@gmail.com"),
     "informations.phones" -> Seq("01.23.45.67.89", "98.76.54.32.10"))

  val validJson = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "Tournay",
    "age" -> 27,
    "informations" -> Json.obj(
      "label" -> "Personal",
      "email" -> "fakecontact@gmail.com",
      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10")))

  val invalidJson = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "Tournay",
    "age" -> 27,
    "informations" -> Json.obj(
      "label" -> "",
      "email" -> "fakecontact@gmail.com",
      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10")))

  import play.api.data.validation2._
  import Extractors._
  import Constraints._

  type M = Map[String, Seq[String]]
  type J = JsValue

  "Map / Json Validation" should {

    "extract data" in {

      (Path \ "firstname").validate[M, Seq[String]].validate(validMap) mustEqual(Success(Seq("Julien")))
      (Path \ "firstname").validate[J, String].validate(validJson) mustEqual(Success("Julien"))

      val errPath = Path \ "foo"
      val error = Failure(Seq(errPath -> Seq("validation.required")))
      errPath.validate[M, String].validate(invalidMap)  mustEqual(error)
      errPath.validate[J, String].validate(invalidJson) mustEqual(error)
    }

    "validate data" in {
      (Path \ "firstname").validate[M, String](nonEmptyText).validate(validMap) mustEqual(Success("Julien"))
      (Path \ "firstname").validate[J, String](nonEmptyText).validate(validJson) mustEqual(Success("Julien"))

      val p = (Path \ "informations" \ "label")
      p.validate[M, String](nonEmptyText).validate(invalidMap)  mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
      p.validate[J, String](nonEmptyText).validate(invalidJson) mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
    }

    "validate deep" in {

      val p = (Path \ "informations" \ "label")

      (Path \ "informations").validate[M, String](
        (Path \ "label").validate[M, String](nonEmptyText)).validate(validMap) mustEqual(Success("Personal"))

      (Path \ "informations").validate[J, String](
        (Path \ "label").validate[J, String](nonEmptyText)).validate(validJson) mustEqual(Success("Personal"))

      (Path \ "informations").validate[M, String](
        (Path \ "label").validate[M, String](nonEmptyText)).validate(invalidMap) mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))

      (Path \ "informations").validate[J, String](
        (Path \ "label").validate[J, String](nonEmptyText)).validate(invalidJson) mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
    }


    "coerce type" in {

      (Path \ "age").validate[M, Int].validate(validMap) mustEqual(Success(27))
      (Path \ "age").validate[J, Int].validate(validJson) mustEqual(Success(27))

      (Path \ "firstname").validate[M, Int].validate(validMap) mustEqual(Failure(Seq((Path \ "firstname") -> Seq("validation.int"))))
      (Path \ "firstname").validate[J, Int].validate(validJson) mustEqual(Failure(Seq((Path \ "firstname") -> Seq("validation.int"))))
    }


    "compose constraints" in {
      // TODO: create MonoidOps
      import Validations._
      val composed = monoidConstraint.append(nonEmptyText, minLength(3))

      (Path \ "firstname").validate[M, String](composed).validate(validMap) mustEqual(Success("Julien"))
      (Path \ "firstname").validate[J, String](composed).validate(validJson) mustEqual(Success("Julien"))

      val p = Path \ "informations" \ "label"
      val err = Failure(Seq(p -> Seq("validation.nonemptytext", "validation.minLength")))

      p.validate[M, String](composed).validate(invalidMap) mustEqual(err)
      p.validate[J, String](composed).validate(invalidJson) mustEqual(err)
    }


    "compose validations" in {
      import play.api.libs.functional.syntax._
      import Validations._

      ((Path \ "firstname").validate[M, String](nonEmptyText) ~
        (Path \ "lastname").validate[M, String](nonEmptyText)){ _ -> _ }
          .validate(validMap) mustEqual Success("Julien" -> "Tournay")

      ((Path \ "firstname").validate[J, String](nonEmptyText) ~
        (Path \ "lastname").validate[J, String](nonEmptyText)){ _ -> _ }
          .validate(validJson) mustEqual Success("Julien" -> "Tournay")

      ((Path \ "firstname").validate[M, String](nonEmptyText) ~
      (Path \ "lastname").validate[M, String](nonEmptyText) ~
      (Path \ "informations" \ "label").validate[M, String](nonEmptyText)){ (_, _, _) }
          .validate(invalidMap) mustEqual Failure(Seq((Path \ "informations" \ "label") -> Seq("validation.nonemptytext")))


    ((Path \ "firstname").validate[J, String](nonEmptyText) ~
    (Path \ "lastname").validate[J, String](nonEmptyText) ~
    (Path \ "informations" \ "label").validate[J, String](nonEmptyText)){ (_, _, _) }
        .validate(invalidJson) mustEqual Failure(Seq((Path \ "informations" \ "label") -> Seq("validation.nonemptytext")))

      success
    }

  }

}