package atc.llm

import atc.config.{ModelConfig, ModelSpec, Tokens}

import com.openai.client.OpenAIClient
import com.openai.core.{JsonValue, RequestOptions, Timeout}
import com.openai.errors.BadRequestException

import java.io.IOException
import java.util.Locale
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Shared by the two OpenAI-shaped adapters (Chat Completions and Responses):
  * the client, the vendor `thinking` switch, and the guessed lowest reasoning
  * effort for non-thinking calls with its one-time fallback. */
private[llm] abstract class OpenAIShapedModel(spec: ModelSpec) extends SpecModel(spec):
  private final case class Connection(client: OpenAIClient, http: okhttp3.OkHttpClient)
  private var opened: Option[Connection] = None
  private def connection: Connection = synchronized:
    opened.getOrElse:
      val timeout = Timeout.builder().request(Providers.RequestTimeout).build()
      val plain = Providers.httpClient(timeout.connect(), timeout.read(), timeout.write(), timeout.request())
      // The provider's headers go on every request, model lists included, before the authorization.
      // One OkHttp refuses fails the call as an `IOException`: any other exception from an
      // asynchronous call is thrown again on OkHttp's dispatcher thread.
      val headers: okhttp3.Interceptor = chain =>
        val request = chain.request().newBuilder()
        for (name, value) <- Providers.headers(spec) do
          try request.header(name, value)
          catch case _: IllegalArgumentException => throw IOException(s"The provider header $name is not valid HTTP")
        chain.proceed(request.build())
      val http = (headers :: authorization.toList).foldLeft(plain.newBuilder())(_.addInterceptor(_)).build()
      val created = Connection(Providers.openAiClient(spec, com.openai.client.okhttp.OkHttpClient(http)), http)
      opened = Some(created)
      created
  protected def client: OpenAIClient = connection.client
  /** The model's HTTP client, for requests the SDK does not make. */
  protected def http: okhttp3.OkHttpClient = connection.http
  /** Authorizes every request when the provider's credential is not a fixed key. */
  protected def authorization: Option[okhttp3.Interceptor] = None
  protected def knownEfforts: List[String] = ModelConfig.ReasoningEfforts

  /** A tool's parameter schema as the members of the SDK's parameters object. */
  protected def schema(t: ToolSpec): java.util.Map[String, JsonValue] =
    ujson.read(t.parametersJson).obj.map((k, v) => k -> JsonValue.from(SdkJson.toJava(v))).asJava

  protected def streamingClient: OpenAIClient =
    val current = connection
    val transport = com.openai.client.okhttp.OkHttpClient(ModelRequest.scopedHttpClient(current.http))
    current.client.withOptions(_.httpClient(Providers.borrowed(transport)).timeout(Providers.StreamTimeout))
  override def close(): Unit = synchronized:
    opened.foreach(_.client.close())
    opened = None

  /** Set once the model rejected the reasoning-effort parameter: it takes no such parameter. */
  @volatile private var effortRejected = false

  /** The reasoning effort a request sends: the current [[effort]] for a thinking
    * call. A non-thinking call sends the lowest effort the model takes when
    * guessing one makes sense: the model is known to reason, has no `thinking`
    * switch, and has not rejected the parameter. */
  protected def requestEffort(thinking: Boolean): Option[String] =
    if thinking then effort
    else if effortRejected || settings.thinking.isDefined then None
    else Providers.lowestEffort(modelId, effort.isDefined)

  /** `thinking: {"type": "enabled"|"disabled"}`, the switch of the
    * OpenAI-compatible vendors that have one (DeepSeek, GLM, Kimi, MiniMax).
    * Sent only when the config sets `thinking` for the model: OpenAI itself
    * rejects the parameter. A non-thinking call always says `disabled`. */
  protected def thinkingSwitch(thinking: Boolean): Option[JsonValue] =
    settings.thinking.map(on => Providers.thinkingSwitch(thinking && on))

  /** `GET /models`. Besides the id, reads the context window that OpenRouter
    * (`context_length`), vLLM (`max_model_len`) and DeepSeek (`context_window`) report,
    * the `name` OpenRouter and DeepSeek give, and the efforts DeepSeek lists with its
    * default. A reported output limit is not taken: it would be sent with every request
    * and reserved from the window. */
  private[atc] def listModels(): List[ModelSpec] =
    val options = RequestOptions.builder().timeout(Providers.ListTimeout).build()
    client.models().list(options).items().asScala.toList.map: m =>
      // Gemini's compatible list names models `models/<id>`; its chat endpoint takes the bare id.
      val id = m.id().stripPrefix("models/")
      val extra = m._additionalProperties().asScala
      def number(key: String) = extra.get(key).flatMap(_.asNumber().toScala).map(_.longValue)
      val window =
        number("context_length").orElse(number("max_model_len")).orElse(number("context_window")).flatMap(Tokens.from)
      val name = extra.get("name").flatMap(_.asString().toScala).map(_.trim).filter(_.nonEmpty)
      val effort = extra.get("effort").flatMap(_.asObject().toScala).map(_.asScala)
      def level(v: JsonValue) = v.asString().toScala.map(_.toLowerCase(Locale.ROOT))
      val efforts = effort.flatMap(_.get("supported_levels")).flatMap(_.asArray().toScala)
        .map(_.asScala.toList.flatMap(level).filter(ModelConfig.ReasoningEfforts.contains)).filter(_.nonEmpty)
      val reasoning = effort.flatMap(_.get("default_level")).flatMap(level).filter(e => efforts.forall(_.contains(e)))
      val settings = spec.settings.copy(contextWindow = window, displayName = name)
      spec.listed(
        id,
        settings.copy(efforts = efforts.orElse(settings.efforts), reasoning = reasoning.orElse(settings.reasoning))
      )

  /** Send `request` with `effort` (the reasoning setting of a one-shot call).
    * When that was a guessed lowest effort (a non-thinking call) and the model
    * rejects it, remember that and ask again without it. */
  protected def withEffortFallback[E, R](thinking: Boolean, effort: Option[E])(request: Option[E] => R): R =
    try request(effort)
    catch
      case e: BadRequestException
          if !thinking && effort.isDefined &&
            Providers.isReasoningEffortRejection(e.param().toScala, Option(e.getMessage).getOrElse("")) =>
        effortRejected = true
        request(None)
