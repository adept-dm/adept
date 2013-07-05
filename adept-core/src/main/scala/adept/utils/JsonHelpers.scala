package adept.utils

import java.io.StringWriter
import java.io.PrintWriter
import org.json4s._
import org.json4s.native.JsonMethods._

object JsonHelpers extends Logging {

  def ifNonEmpty(f: JField): Option[JField] = {
    val (key, value) = f
    value match {
      case JObject(obj) if obj.isEmpty => None
      case JArray(arr) if arr.isEmpty => None
      case _ => Some(f)
    }
  }

  def asJObject(l: Option[JField]*): JObject = {
    JObject(List[Option[JField]](l: _*).flatten)
  }

  trait JsonTransformer[T] {
    def eitherOf(json: JValue, s: String): Either[String, T]
  }

  def issueError[A](json: JValue, s: String, expected: String, got: JValue): Either[String, A] = {
    if (logger.isDebugEnabled) {
      val e = new Exception()
      val w = new StringWriter()
      e.printStackTrace(new PrintWriter(w))
      logger.debug("printing stacktrace for debug: " + w.getBuffer().toString)
      logger.debug("pure json was: " + json)
    }
    Left("expected a " + expected + " but found:" + got + " for " + s + " in:" + pretty(render(json)))
  }

  implicit val jstringTransformer = new JsonTransformer[String] {
    override def eitherOf(json: JValue, s: String): Either[String, String] = {
      (json \ s) match {
        case JString(v) => Right(v)
        case a => issueError(json, s, "JString", a)
      }
    }
  }

  implicit val booleanTransformer = new JsonTransformer[Boolean] {
    override def eitherOf(json: JValue, s: String): Either[String, Boolean] = {
      (json \ s) match {
        case JBool(v) => Right(v)
        case a => issueError(json, s, "JBool", a)
      }
    }
  }
  
  def eitherOf[T](json: JValue)(implicit t: JsonTransformer[T]) = new {
    def \(s: String): Either[String, T] = {
      t.eitherOf(json, s)
    }
  }

  def getOptionalValue[A: JsonTransformer](json: JValue, name: String): Either[String, Option[A]] = {
    val maybeValue = (json \ name).toOption //is Some if value exists

    //if value exists it should be of the expected type A, and wrapped in Option
    val mappedValue = maybeValue.map(_ => (eitherOf[A](json) \ name).right.map(a => Some(a)))

    mappedValue.getOrElse(Right(None)) //if no value is fine this is Ok
  }

  def getOptionalStrings(json: JValue, name: String): Either[String, Set[String]] = {
    (json \ name).toOption.map { foundJson =>
      readSet(foundJson)(array => array.map { stringJson =>
        stringJson match {
          case JString(v) => Right(v)
          case a => issueError(json, name, "JString", a)
        }
      })
    }.getOrElse {
      Right(Set.empty)
    }
  }

  def readSet[A](json: JValue)(f: List[JValue] => Seq[Either[String, A]]): Either[String, Set[A]] = {
    readSeq(json)(f).right.map(_.toSet)
  }

  def readSeq[A](json: JValue)(f: List[JValue] => Seq[Either[String, A]]): Either[String, Seq[A]] = {
    json match {
      case JArray(list) =>
        EitherUtils.reduce(f(list))
      case JNothing => Right(Seq())
      case somethingElse => {
        issueError(json, "JArray", "an array", somethingElse)
      }
    }
  }

}