import io.circe.parser.parse
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.UriContext
import sttp.client3.testing.SttpBackendStub
import sttp.tapir.generated.TapirGeneratedEndpoints
import sttp.tapir.generated.TapirGeneratedEndpoints._
import sttp.tapir.server.stub.TapirStubInterpreter

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class JsonRoundtrip extends AnyFreeSpec with Matchers {

  // the endpoint echoes the decoded body straight back, and the decoded query params in the x-* headers,
  // so the response is a readout of what decoding produced
  private val route = TapirGeneratedEndpoints.createBook.serverLogic[Future] { case (required, optionalDefault, book) =>
    Future successful Right[String, (Book, String, String)]((book, required, optionalDefault))
  }
  private val stub = TapirStubInterpreter(SttpBackendStub.asynchronousFuture).whenServerEndpoint(route).thenRunLogic().backend()

  private def normalise(json: String): String = parse(json).toTry.get.noSpacesSortKeys

  private def send(requestBody: String, query: Map[String, String] = Map("required" -> "a-required-value")): (Int, String, Option[String], Option[String]) = Await.result(
    sttp.client3.basicRequest
      .post(uri"http://test.com/books?$query")
      .body(requestBody)
      .send(stub)
      .map(resp => (resp.code.code, resp.body.fold(identity, normalise), resp.header("x-required"), resp.header("x-optional-default"))),
    1.second
  )

  private val allDefaults = normalise("""{
    "title": "A title",
    "description": "This is the default description",
    "subtitle": "A default subtitle",
    "pageCount": 42,
    "available": true,
    "tags": [],
    "quote": "He said \"hi\"",
    "isbn": "unknown"
  }""")

  "absent properties decode to their declared defaults" in {
    // `isbn` is absent too, and still decodes despite being listed as required, because the scala default applies first
    val (code, body, _, _) = send("""{"title": "A title"}""")
    code shouldEqual 200
    body shouldEqual allDefaults
  }

  "an explicit null on a nullable property is treated as an absent field, so the default applies" in {
    val (code, body, _, _) = send("""{"title": "A title", "subtitle": null}""")
    code shouldEqual 200
    // NB this differs from circe: jsoniter-scala does not distinguish an explicit null from a missing key for a
    // property carrying a scala default -- both decode to the default, so None is unreachable from the wire
    body shouldEqual allDefaults
  }

  "a value supplied for a nullable property wins over its default" in {
    val (code, body, _, _) = send("""{"title": "A title", "subtitle": "A supplied subtitle"}""")
    code shouldEqual 200
    body shouldEqual normalise("""{
      "title": "A title",
      "description": "This is the default description",
      "subtitle": "A supplied subtitle",
      "pageCount": 42,
      "available": true,
      "tags": [],
      "quote": "He said \"hi\"",
      "isbn": "unknown"
    }""")
  }

  "supplied values win over defaults" in {
    val supplied = """{
      "title": "A title",
      "description": "A supplied description",
      "subtitle": "A supplied subtitle",
      "pageCount": 7,
      "available": false,
      "tags": ["fiction", "classics"],
      "quote": "A supplied quote",
      "isbn": "978-0000000000"
    }"""
    val (code, body, _, _) = send(supplied)
    code shouldEqual 200
    // nothing falls back to a default, so the body round-trips unchanged
    body shouldEqual normalise(supplied)
  }

  "query params" - {
    val minimalBody = """{"title": "A title"}"""

    "an absent optional query param decodes to its declared default" in {
      val (code, _, required, optionalDefault) = send(minimalBody, Map("required" -> "a-required-value"))
      code shouldEqual 200
      required shouldEqual Some("a-required-value")
      optionalDefault shouldEqual Some("some query param")
    }

    "a supplied optional query param wins over its default" in {
      val (code, _, required, optionalDefault) =
        send(minimalBody, Map("required" -> "a-required-value", "optional-default" -> "a supplied value"))
      code shouldEqual 200
      required shouldEqual Some("a-required-value")
      optionalDefault shouldEqual Some("a supplied value")
    }

    "an absent required query param without a default is rejected" in {
      val (code, _, _, _) = send(minimalBody, Map.empty)
      code shouldEqual 400
    }
  }
}
