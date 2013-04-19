package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

class WritesSpec extends Specification {

  import models._

  val contact = Contact("Julien", "Tournay", None, Seq(
    ContactInformation("Personnal", Some("fakecontact@gmail.com"), Seq("01.23.45.67.89", "98.76.54.32.10"))))

  val contactMap = Map(
    "firstname" -> Seq("Julien"),
    "lastname" -> Seq("Tournay"),
    "age" -> Seq("27"),
    "informations[0].label" -> Seq("Personal"),
    "informations[0].email" -> Seq("fakecontact@gmail.com"),
    "informations[0].phones[0]" -> Seq("01.23.45.67.89"),
    "informations[0].phones[1]" -> Seq("98.76.54.32.10"))

  val contactJson = Json.obj(
    "firstname" -> "Julien",
    "lastname" -> "Tournay",
    "age" -> 27,
    "informations" -> Json.obj(
      "label" -> "Personal",
      "email" -> "fakecontact@gmail.com",
      "phones" -> Seq("01.23.45.67.89", "98.76.54.32.10")))


  import play.api.data.validation2._
  import Writes._

  type M = Map[String, Seq[String]]
  type J = JsValue

  "Writes" should {

    "write Map" in {
      implicit def contactWrite = {

        val __ = Path[M]()

        import play.api.libs.functional.syntax.unlift
        implicit val infoWrites =
          ((__ \ "label").write[String] ~
          (__ \ "email").write[Option[String]] ~
          (__ \ "phones").write[Seq[String]]) (unlift(ContactInformation.unapply _))

        ((__ \ "firstname").write[String] ~
        (__ \ "lastname").write[String] ~
        (__ \ "company").write[Option[String]] ~
        (__ \ "informations").write[Seq[ContactInformation]]) (unlift(Contact.unapply _))
      }

      Writes(contact) mustEqual contactMap
    }
  }

}