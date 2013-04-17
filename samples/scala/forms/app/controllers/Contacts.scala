package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import play.api.libs.json._

import views._

import models._

import play.api.data.validation2.Path

object Contacts extends Controller {

  /**
   * Contact Form definition.
   */
  val contactForm: Form[Contact] = Form(

    // Defines a mapping that will handle Contact values
    mapping(
      "firstname" -> nonEmptyText,
      "lastname" -> nonEmptyText,
      "company" -> optional(text),

      // Defines a repeated mapping
      "informations" -> seq(
        mapping(
          "label" -> nonEmptyText,
          "email" -> optional(email),
          "phones" -> seq(
            text verifying pattern("""[0-9.+]+""".r, error="A valid phone number is required")
          )
        )(ContactInformation.apply)(ContactInformation.unapply)
      )

    )(Contact.apply)(Contact.unapply)
  )

  import play.api.data.validation2._
  import Mappings._
  import Constraints._
  import Validations._

  def contactJson = {
    val __ = Path[JsValue]()

    val infoValidation =
      ((__ \ "label").validate(nonEmptyText) ~
      (__ \ "email").validate(optional(email)) ~
      (__ \ "phones").validate(seq(pattern("""[0-9.+]+""".r)))) (ContactInformation.apply _)

    ((__ \ "firstname").validate(nonEmptyText) ~
    (__ \ "lastname").validate(nonEmptyText) ~
    (__ \ "company").validate[Option[String]] ~
    (__ \ "informations").validate(seq(infoValidation))) (Contact.apply _)
  }

  // TODO: avoid code repetition
  def contactMap = {
    val __ = Path[Map[String, Seq[String]]]()

    val infoValidation =
      ((__ \ "label").validate(nonEmptyText) ~
      (__ \ "email").validate(optional(email)) ~
      (__ \ "phones").validate(seq(pattern("""[0-9.+]+""".r)))) (ContactInformation.apply _)

    ((__ \ "firstname").validate(nonEmptyText) ~
    (__ \ "lastname").validate(nonEmptyText) ~
    (__ \ "company").validate[Option[String]] ~
    (__ \ "informations").validate(seq(infoValidation))) (Contact.apply _)
  }


  /**
   * Display an empty form.
   */
  def form = Action {
    Ok(html.contact.form(contactForm));
  }

  /**
   * Display a form pre-filled with an existing Contact.
   */
  def editForm = Action {
    val existingContact = Contact(
      "Fake", "Contact", Some("Fake company"), informations = List(
        ContactInformation(
          "Personal", Some("fakecontact@gmail.com"), List("01.23.45.67.89", "98.76.54.32.10")
        ),
        ContactInformation(
          "Professional", Some("fakecontact@company.com"), List("01.23.45.67.89")
        ),
        ContactInformation(
          "Previous", Some("fakecontact@oldcompany.com"), List()
        )
      )
    )
    Ok(html.contact.form(contactForm.fill(existingContact)))
  }

  private def negotiate: BodyParser[Either[Map[String,Seq[String]], JsValue]] = parse.using{ r =>
    r.contentType match {
      case Some("text/json" | "application/json") => parse.json.map(Right(_))
      case _ => parse.urlFormEncoded.map(Left(_))
    }
  }

  /**
   * Handle form submission.
   */
  import play.api.data.validation2.Validations.pathWrite

  // curl http://localhost:9000/contacts -XPOST -H "Content-Type: application/json" -d "{\"firstname\":\"Julien\",\"lastname\":\"Tournay\",\"age\":27,\"informations\":[{\"label\":\"Personal\",\"email\":\"fakecontact@gmail.com\",\"phones\":[\"01.23.45.67.89\",\"98.76.54.32.10\"]}]}" -i
  // OR
  // curl http://localhost:9000/contacts -XPOST -d "firstname=Julien&lastname=Tournay&age=27&informations[0].label=Personal&informations[0].email=fakecontact@gmail.com&informations[0].phones[0]=01.23.45.67.89&informations[0].phones[1]=98.76.54.32.10" -i
  def submit = Action(negotiate) { implicit request =>
    request.body.fold(
      form => {
        println(form)
        println(contactMap.validate(form))
        NotImplemented
      },
      json =>
        contactJson.validate(json).fold(
          err => BadRequest(Json.toJson(err)),
          _ => Ok))
    //contactForm.bindFromRequest.fold(
    //  errors => BadRequest(html.contact.form(errors)),
    //  contact => Ok(html.contact.summary(contact))
    //)
  }

}