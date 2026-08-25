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

  // the endpoint echoes the decoded body straight back, so the response is a readout of what decoding produced
  private val route = TapirGeneratedEndpoints.createBook.serverLogic[Future](book => Future successful Right[String, Book](book))
  private val stub = TapirStubInterpreter(SttpBackendStub.asynchronousFuture).whenServerEndpoint(route).thenRunLogic().backend()

  private def normalise(json: String): String = parse(json).toTry.get.noSpacesSortKeys

  private def post(requestBody: String): (Int, String) = Await.result(
    sttp.client3.basicRequest
      .post(uri"http://test.com/books")
      .body(requestBody)
      .send(stub)
      .map(resp => resp.code.code -> resp.body.fold(identity, normalise)),
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
    val (code, body) = post("""{"title": "A title"}""")
    code shouldEqual 200
    body shouldEqual allDefaults
  }

  "an explicit null on a nullable property is treated as an absent field, so the default applies" in {
    val (code, body) = post("""{"title": "A title", "subtitle": null}""")
    code shouldEqual 200
    // NB this differs from circe: zio-json does not distinguish an explicit null from a missing key for a
    // property carrying a scala default -- both decode to the default, so None is unreachable from the wire
    body shouldEqual allDefaults
  }

  "a value supplied for a nullable property wins over its default" in {
    val (code, body) = post("""{"title": "A title", "subtitle": "A supplied subtitle"}""")
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
    val (code, body) = post(supplied)
    code shouldEqual 200
    // nothing falls back to a default, so the body round-trips unchanged
    body shouldEqual normalise(supplied)
  }
}
