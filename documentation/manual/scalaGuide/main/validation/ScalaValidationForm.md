# Form template helpers

The validation API is compatible with play's existing [[template helpers|ScalaFormHelpers]].
There's one difference though : **you have to use `play.api.data.mapping.Form`** instead of using `play.api.data.Form`.

This `Form` helpers use `Rule` to validate data, and `Write` to format form data:

## Fill a form with initial default values

Consider the following example, from play's sample "computer database" application :

The `edit` action renders a form pre-filled with `computer` data. All we have to do is to create a `Form` instance using `Form.fill`:

```scala
def edit(id: Long) = Action {
  Computer.findById(id).map { computer =>
    Ok(html.editForm(id, Form.fill(computer), Company.options))
  }.getOrElse(NotFound)
}
```

> Note we are using `play.api.data.mapping.Form`, **NOT** `play.api.data.Form`

Note that `Form.fill` needs to find an implicit `Write[Computer, UrlFormEncoded]`. In this example, we define it in `Application.scala`:

```scala
implicit val computerW = To[UrlFormEncoded] { __ =>
  import play.api.data.mapping.Writes._
  ((__ \ "id").write[Pk[Long]] ~
   (__ \ "name").write[String] ~
   (__ \ "introduced").write(option(date("yyyy-MM-dd"))) ~
   (__ \ "discontinued").write(option(date("yyyy-MM-dd"))) ~
   (__ \ "company").write[Option[Long]]) (unlift(Computer.unapply _))
}
```

Not only the write object serializes primitive types but it formats data when needed. 
In our example, dates will be displayed in the "yyyy-MM-dd" format.

`Form.fill` writes a `Computer` using `computerW`, and the resulting `Map[String, Seq[String]]` is then used by the `editForm` templates:

```scala
@(id: Long, computerForm: play.api.data.mapping.Form[Computer], companies : Seq[(String, String)])

@main {
	// form definition
}
```

From there, all the [[template helpers|ScalaFormHelpers]] work exactly as they used to :

```scala
@inputText(computerForm("name"), '_label -> "Computer name")
```

## Binding form data

The Form object uses a `Rule` to bind and validate data from a request body :

```scala
def update(id: Long) = Action(parse.urlFormEncoded) { implicit request =>
  val r = computerValidation.validate(request.body)
  r match {
    case Failure(_) => BadRequest(html.editForm(id, Form(request.body, r), Company.options))
    case Success(computer) => {
      Computer.update(id, computer)
      Home.flashing("success" -> "Computer %s has been updated".format(computer.name))
    }
  }
}
```

The Rule `computerValidation` is defined below.
Any custom format should match the `Form.fill` definition and be specified as a `read()` option.

```scala
implicit val computerValidation = From[UrlFormEncoded] { __ =>
    import play.api.data.mapping.Rules._
    ((__ \ "id").read(ignored[UrlFormEncoded, Pk[Long]](NotAssigned)) ~
     (__ \ "name").read(notEmpty) ~
     (__ \ "introduced").read(option(date("yyyy-MM-dd"))) ~
     (__ \ "discontinued").read(option(date("yyyy-MM-dd"))) ~
     (__ \ "company").read[Option[Long]]) (Computer.apply _)
  }
```

> **Next:** - [[Validation Inception|ScalaValidationMacros]]