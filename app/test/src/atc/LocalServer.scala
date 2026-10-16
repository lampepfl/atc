package atc

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Locale
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.jdk.CollectionConverters.*

/** A local HTTP server that stands in for a provider, so no test reaches the network. */
object LocalServer:
  /** A request as the server received it. Header names are lower case. */
  final case class Request(method: String, path: String, query: String, headers: Map[String, String], body: String):
    def form: Map[String, String] = body.split('&').toList.filter(_.nonEmpty).map { pair =>
      val (k, v) = pair.span(_ != '=')
      java.net.URLDecoder.decode(k, UTF_8).nn -> java.net.URLDecoder.decode(v.drop(1), UTF_8).nn
    }.toMap
    def json: ujson.Value = ujson.read(body)

  /** Run `test` with the base URL of a server on which `handle` answers every request,
    * each on a thread of its own, and stop the server afterwards. */
  def withHandler(handle: HttpExchange => Unit)(test: String => Unit): Unit =
    val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0).nn
    val executor = Executors.newCachedThreadPool()
    server.setExecutor(executor)
    server.createContext("/", exchange => handle(exchange))
    server.start()
    try test(s"http://127.0.0.1:${server.getAddress.nn.getPort}")
    finally
      server.stop(0)
      executor.shutdownNow()

  /** Run `test` against a server that records every request and answers it with the
    * status, content type and body `respond` gives; `test` gets the base URL and the
    * requests so far. */
  def withServer(respond: Request => (Int, String, String))(test: (String, () => List[Request]) => Unit): Unit =
    val requests = ConcurrentLinkedQueue[Request]()
    withHandler { exchange =>
      try
        val uri = exchange.getRequestURI.nn
        val headers = exchange.getRequestHeaders.nn.asScala
          .map((name, values) => name.toLowerCase(Locale.ROOT) -> values.asScala.mkString(","))
          .toMap
        val request = Request(
          exchange.getRequestMethod.nn,
          uri.getPath.nn,
          Option(uri.getRawQuery).getOrElse(""),
          headers,
          String(exchange.getRequestBody.nn.readAllBytes().nn, UTF_8),
        )
        requests.add(request)
        val (status, contentType, reply) = respond(request)
        val bytes = reply.getBytes(UTF_8)
        exchange.getResponseHeaders.nn.add("Content-Type", contentType)
        exchange.sendResponseHeaders(status, bytes.length.toLong)
        exchange.getResponseBody.nn.write(bytes)
      finally exchange.close()
    }(url => test(url, () => requests.asScala.toList))
