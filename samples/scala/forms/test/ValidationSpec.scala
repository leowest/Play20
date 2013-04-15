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
  import Mappings._
  import Constraints._

  type M = Map[String, Seq[String]]
  type J = JsValue


  "Form Validation" should {

    val __ = Path[M]()
    val valid = validMap
    val invalid = invalidMap

    "extract data" in {

      (__ \ "firstname").validate[String]
        .validate(valid) mustEqual(Success("Julien"))

      val errPath = __ \ "foo"
      val error = Failure(Seq(errPath -> Seq("validation.required")))
      errPath.validate[String].validate(invalid)  mustEqual(error)
    }

    "validate data" in {
      (__ \ "firstname").validate(nonEmptyText).validate(valid) mustEqual(Success("Julien"))

      val p = (__ \ "informations" \ "label")
      p.validate(nonEmptyText).validate(invalid)  mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
    }

    "validate optional" in {
      (__ \ "firstname").validate[Option[String]].validate(valid) mustEqual(Success(Some("Julien")))
      (__ \ "foobar").validate[Option[String]].validate(valid) mustEqual(Success(None))
    }

    "validate deep" in {
      val p = (__ \ "informations" \ "label")

      (__ \ "informations").validate(
        (__ \ "label").validate(nonEmptyText)).validate(valid) mustEqual(Success("Personal"))

      (__ \ "informations").validate(
        (__ \ "label").validate(nonEmptyText)).validate(invalid) mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
    }


    "coerce type" in {
      (__ \ "age").validate[Int].validate(valid) mustEqual(Success(27))
      (__ \ "firstname").validate[Int].validate(valid) mustEqual(Failure(Seq((__ \ "firstname") -> Seq("validation.int"))))
    }

    "compose constraints" in {
      // TODO: create MonoidOps
      import Validations._
      val composed = monoidConstraint.append(nonEmptyText, minLength(3))
      (__ \ "firstname").validate(composed).validate(valid) mustEqual(Success("Julien"))

      val p = __ \ "informations" \ "label"
      val err = Failure(Seq(p -> Seq("validation.nonemptytext", "validation.minLength")))
      p.validate(composed).validate(invalid) mustEqual(err)
    }


    "compose validations" in {
      import play.api.libs.functional.syntax._
      import Validations._

      ((__ \ "firstname").validate(nonEmptyText) ~
        (__ \ "lastname").validate(nonEmptyText)){ _ -> _ }
          .validate(valid) mustEqual Success("Julien" -> "Tournay")

      ((__ \ "firstname").validate(nonEmptyText) ~
      (__ \ "lastname").validate(nonEmptyText) ~
      (__ \ "informations" \ "label").validate(nonEmptyText)){ (_, _, _) }
        .validate(invalid) mustEqual Failure(Seq((__ \ "informations" \ "label") -> Seq("validation.nonemptytext")))
    }
  }

  "Json Validation" should {

    val __ = Path[J]()
    val valid = validJson
    val invalid = invalidJson

    "extract data" in {

      (__ \ "firstname").validate[String]
        .validate(valid) mustEqual(Success("Julien"))

      val errPath = __ \ "foo"
      val error = Failure(Seq(errPath -> Seq("validation.required")))
      errPath.validate[String].validate(invalid)  mustEqual(error)
    }

    "validate data" in {
      (__ \ "firstname").validate(nonEmptyText).validate(valid) mustEqual(Success("Julien"))

      val p = (__ \ "informations" \ "label")
      p.validate(nonEmptyText).validate(invalid)  mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
    }

    "validate optional" in {
      (__ \ "firstname").validate[Option[String]].validate(valid) mustEqual(Success(Some("Julien")))
      (__ \ "foobar").validate[Option[String]].validate(valid) mustEqual(Success(None))
    }

    "validate deep" in {
      val p = (__ \ "informations" \ "label")

      (__ \ "informations").validate(
        (__ \ "label").validate(nonEmptyText)).validate(valid) mustEqual(Success("Personal"))

      (__ \ "informations").validate(
        (__ \ "label").validate(nonEmptyText)).validate(invalid) mustEqual(Failure(Seq(p -> Seq("validation.nonemptytext"))))
    }

    "coerce type" in {
      (__ \ "age").validate[Int].validate(valid) mustEqual(Success(27))
      (__ \ "firstname").validate[Int].validate(valid) mustEqual(Failure(Seq((__ \ "firstname") -> Seq("validation.int"))))
    }

    "compose constraints" in {
      // TODO: create MonoidOps
      import Validations._
      val composed = monoidConstraint.append(nonEmptyText, minLength(3))
      (__ \ "firstname").validate(composed).validate(valid) mustEqual(Success("Julien"))

      val p = __ \ "informations" \ "label"
      val err = Failure(Seq(p -> Seq("validation.nonemptytext", "validation.minLength")))
      p.validate(composed).validate(invalid) mustEqual(err)
    }

    "compose validations" in {
      import play.api.libs.functional.syntax._
      import Validations._

      ((__ \ "firstname").validate(nonEmptyText) ~
        (__ \ "lastname").validate(nonEmptyText)){ _ -> _ }
          .validate(valid) mustEqual Success("Julien" -> "Tournay")

      ((__ \ "firstname").validate(nonEmptyText) ~
      (__ \ "lastname").validate(nonEmptyText) ~
      (__ \ "informations" \ "label").validate(nonEmptyText)){ (_, _, _) }
        .validate(invalid) mustEqual Failure(Seq((__ \ "informations" \ "label") -> Seq("validation.nonemptytext")))
    }
  }

}