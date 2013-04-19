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

  import play.api.libs.functional.Monoid

  import play.api.data.validation2._
  import Mappings._
  import Constraints._
  import Validations._

  import Writes._

  def contactValidation[I](implicit i1: Path[I] => Mapping[String, I, String], i2: Path[I] => Mapping[String, I, Seq[String]], i3: Path[I] => Mapping[String, I, Seq[I]]) = {
    val __ = Path[I]()

    val infoValidation =
      ((__ \ "label").validate(nonEmptyText) ~
      (__ \ "email").validate(optional(email)) ~
      (__ \ "phones").validate(seq(pattern("""[0-9.+]+""".r)))) (ContactInformation.apply _)

    ((__ \ "firstname").validate(nonEmptyText) ~
    (__ \ "lastname").validate(nonEmptyText) ~
    (__ \ "company").validate[Option[String]] ~
    (__ \ "informations").validate(seq(infoValidation))) (Contact.apply _)
  }

  implicit def contactWrite = {

    val __ = Path[Map[String, Seq[String]]]()


    import play.api.libs.functional.syntax.unlift

   implicit val infoWrites =
      ((__ \ "label").write[String] ~
      (__ \ "email").write[Option[String]] ~
      (__ \ "phones").write[Seq[String]]) (unlift(ContactInformation.unapply _))

    def x(p: Path[Map[String, Seq[String]]]) = writeSeqToMap[ContactInformation](p)
    //implicitly[Path[Map[String, Seq[String]]] => Writes[Seq[ContactInformation], Map[String, Seq[String]]]]

    ((__ \ "firstname").write[String] ~
    (__ \ "lastname").write[String] ~
    (__ \ "company").write[Option[String]] ~
    (__ \ "informations").write[Seq[ContactInformation]]) (unlift(Contact.unapply _))
  }

  import play.api.data.validation2.Form

  /**
   * Display an empty form.
   */
  def form = Action {
    Ok(html.contact.form(Form[Contact]()));
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
    Ok(html.contact.form(Form.fill(existingContact)))
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

  implicit val contactInfo = Json.format[ContactInformation]
  implicit val contactFormat = Json.format[Contact]


  // curl http://localhost:9000/contacts -XPOST -H "Content-Type: application/json" -d "{\"firstname\":\"Julien\",\"lastname\":\"Tournay\",\"age\":27,\"informations\":[{\"label\":\"Personal\",\"email\":\"fakecontact@gmail.com\",\"phones\":[\"01.23.45.67.89\",\"98.76.54.32.10\"]}]}" -i
  // OR
  // curl http://localhost:9000/contacts -XPOST -d "firstname=Julien&lastname=Tournay&age=27&informations[0].label=Personal&informations[0].email=fakecontact@gmail.com&informations[0].phones[0]=01.23.45.67.89&informations[0].phones[1]=98.76.54.32.10" -i
  def submit = Action(negotiate) { implicit request =>
    request.body.fold(
      form =>
        contactValidation[Map[String, Seq[String]]].validate(form).fold(
          err => BadRequest(html.contact.form(Form(form, err))),
          contact => Ok(html.contact.summary(contact))),
      json =>
        contactValidation[JsValue].validate(json).fold(
          err => BadRequest(Json.toJson(err)),
          contact => Ok(Writes[Contact, JsValue](contact))))
  }

}