package atc.llm

import atc.{Debug, Main}
import atc.config.{ModelSpec, ProviderConfig}

import com.openai.client.{OpenAIClient, OpenAIClientImpl}
import com.openai.core.{ClientOptions, JsonValue, RequestOptions, Timeout}
import com.openai.core.http.{HttpClient, HttpRequest}

import java.time.Duration
import java.util.{Locale, UUID}

/** Settings and client construction shared by the provider adapters. */
private[atc] object Providers:
  /** Generous, because a reasoning model with tools can take many minutes. */
  val RequestTimeout: Duration = Duration.ofMinutes(15)

  /** A streamed answer may take longer than [[RequestTimeout]] in all, so a streaming request has
    * no limit on the whole call. Connecting, and each wait for the next bytes, stay bounded, and
    * Ctrl-C cancels the call. */
  val StreamTimeout: Timeout =
    Timeout.builder().read(RequestTimeout).write(RequestTimeout).request(Duration.ZERO).build()

  /** For listing a provider's models, which a user waits for. */
  val ListTimeout: Duration = Duration.ofSeconds(20)

  /** The instructions of a one-shot call that has none, for the providers that require them. */
  val DefaultInstructions = "Answer the request."

  /** How ATC identifies itself; a configured `User-Agent` header replaces it. */
  lazy val UserAgent: String = s"atc/${Main.Version}"

  /** A random id of the current conversation, what `${ATC_SESSION}` in a
    * provider header stands for. */
  @volatile private var conversationId: String = newId()
  private def newId(): String = UUID.randomUUID().toString

  /** The current conversation id. */
  def conversation: String = conversationId

  /** Start a new conversation id (`/new`, or a resumed session). */
  def newConversation(): Unit = conversationId = newId()

  /** The headers of one request to `spec`'s provider: the user agent, then the
    * configured ones with the conversation id filled in. */
  def headers(spec: ModelSpec): Map[String, String] =
    val defaults =
      if spec.headers.keysIterator.exists(_.equalsIgnoreCase("User-Agent")) then Map.empty[String, String]
      else Map("User-Agent" -> UserAgent)
    defaults ++ spec.headers.map: (name, value) =>
      name -> (if value == ProviderConfig.SessionRef then conversationId else value)

  def httpClient(connect: Duration, read: Duration, write: Duration, request: Duration): okhttp3.OkHttpClient =
    okhttp3.OkHttpClient.Builder().retryOnConnectionFailure(false)
      .connectTimeout(connect).readTimeout(read).writeTimeout(write).callTimeout(request).build()

  /** Request clients share the model's connection pool and executor. SDK cleanup, including
    * garbage collection, must not close these resources; the model owns their lifetime. */
  def borrowed(transport: HttpClient): HttpClient =
    new HttpClient:
      def execute(request: HttpRequest, options: RequestOptions) = transport.execute(request, options)
      def executeAsync(request: HttpRequest, options: RequestOptions) = transport.executeAsync(request, options)
      def close(): Unit = ()

  /** Whether a 400 specifically rejects the guessed reasoning-effort setting.
    * Arbitrary bad requests are not retried: that duplicates traffic and can
    * permanently misclassify a model as not supporting effort. */
  def isReasoningEffortRejection(param: Option[String], message: String): Boolean =
    def mentions(value: String): Boolean =
      val s = value.trim.toLowerCase(Locale.ROOT)
      s.contains("reasoning_effort") || s.contains("reasoning.effort") || s.contains("reasoning effort")
    param.exists(p => p.trim.equalsIgnoreCase("reasoning") || mentions(p)) || mentions(message)

  /** Whether a failed request is the provider refusing its web search tool or
    * parameter: a 400 or 422 whose message names it. */
  def isWebSearchRejection(error: Throwable): Boolean =
    Debug.causes(error).exists:
      case e: com.anthropic.errors.AnthropicServiceException => rejectsWebSearch(e.statusCode(), e.getMessage)
      case e: com.openai.errors.OpenAIServiceException => rejectsWebSearch(e.statusCode(), e.getMessage)
      case _ => false

  private def rejectsWebSearch(status: Int, message: String | Null): Boolean =
    val text = Option(message).getOrElse("").toLowerCase(Locale.ROOT)
    (status == 400 || status == 422) &&
    List("web_search", "web search", "websearch", "web-search").exists(text.contains)

  /** The body of a `thinking` switch: `{"type": "enabled"}` / `{"type": "disabled"}`. */
  def thinkingSwitch(on: Boolean): JsonValue =
    JsonValue.from(java.util.Map.of("type", if on then "enabled" else "disabled"))

  /** The lowest `reasoning_effort` an OpenAI model accepts, for a call that
    * should not think: `none` from GPT-5.1 on, `minimal` for the GPT-5 family
    * before it, `low` for the o-series and for anything else the config says
    * takes an effort at all. `None` when the model is not known to reason
    * (sending the parameter to such a model is an error). */
  def lowestEffort(modelId: String, configuredEffort: Boolean): Option[String] =
    val id = modelId.toLowerCase(Locale.ROOT)
    if id.matches("^(o[1-9]|.*/o[1-9]).*") then Some("low")
    else if id.matches("^(.*/)?gpt-5\\.[1-9].*") then Some("none")
    else if id.matches("^(.*/)?gpt-5(-.*)?$") then Some("minimal")
    else if configuredEffort then Some("low")
    else None

  /** The key a client is built with: the configured one, else, against a custom `url` (Ollama,
    * vLLM, LM Studio, ...), a placeholder that such servers ignore. The environment's
    * credentials are for the provider's own API, so the SDK reads them (`None`) only for its
    * default endpoint. `KeyBindings.variables` follows the same rule. */
  def key(spec: ModelSpec): Option[String] = spec.apiKey.orElse(spec.baseUrl.map(_ => "none"))

  /** A client for the two OpenAI-shaped providers, with the [[key]] of `spec`. */
  private[llm] def openAiClient(spec: ModelSpec, transport: HttpClient): OpenAIClient =
    val b = ClientOptions.builder().httpClient(transport).timeout(RequestTimeout)
    key(spec).fold(b.fromEnv())(b.apiKey)
    spec.baseUrl.foreach(b.baseUrl)
    OpenAIClientImpl(b.build())
