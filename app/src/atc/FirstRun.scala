package atc

import atc.config.{KeyBindings, ModelCatalog, ModelSpec, ProviderConfig, ProviderPreset}
import atc.llm.ChatGPTAuth
import atc.ui.Menus

import java.net.{URI, URISyntaxException}
import java.nio.file.Path
import java.time.Duration
import java.util.Locale
import scala.annotation.tailrec
import scala.util.Using
import scala.util.control.NonFatal

/** The first interactive start without a global config: choose one provider,
  * give its key (or sign in, for ChatGPT), and choose a model from the list
  * the provider returns. The key is checked by fetching that list. This object
  * only asks; `Setup.load` writes what the user chose. */
object FirstRun:
  /** What the first run needs from the terminal. */
  trait Ui:
    /** A menu; `None` when cancelled. */
    def choose(title: String, options: List[String]): Option[String]
    /** A secret typed without echo; `None` when empty or cancelled. */
    def askSecret(question: String): Option[String]
    /** A line of text, such as an address; `None` when empty or cancelled. */
    def askText(question: String): Option[String]
    def info(text: String): Unit
    def error(text: String): Unit

  /** A provider at an address the user gives, by the protocol it speaks: its menu label,
    * its `api`, and where its address leads, with an example. */
  final case class CustomServer(label: String, api: String, address: String)

  val CustomServers: List[CustomServer] = List(
    CustomServer(
      "Custom OpenAI-compatible server (Ollama, vLLM, LM Studio, a gateway, …)",
      "openai",
      "the address its /chat/completions and /models go under, such as http://localhost:11434/v1 for Ollama",
    ),
    CustomServer(
      "Custom Anthropic-compatible server",
      "anthropic",
      "the address its /v1/messages and /v1/models go under, such as https://gateway.example.com/anthropic",
    ),
  )

  enum Outcome:
    /** Set up `provider` with `model`, one of the `models` fetched from `endpoint`.
      * `key` is the key the user typed; `None` when one was already bound. */
    case Ready(
      provider: ProviderPreset,
      key: Option[String],
      endpoint: ModelSpec,
      models: List[ModelSpec],
      model: ModelSpec
    )
    /** Write the starting config with every provider and let the user edit it. */
    case ConfigureYourself
    /** Write nothing and run on the built-in defaults. */
    case NotNow

  val ConfigureYourselfLabel = "Configure providers myself (write a starting config and exit)"
  val NotNowLabel = "Not now (use the built-in defaults for this run)"

  /** Ask until the user sets up a provider or chooses one of the other two ways.
    * `keys` are the bindings already in force; `list` fetches a provider's models;
    * `signIn` signs in to a provider that takes no key (ChatGPT), keeping a saved sign-in
    * unless its second argument (`again`) is true. */
  @tailrec def run(
    ui: Ui,
    presets: List[ProviderPreset],
    keys: KeyBindings,
    list: ModelSpec => List[ModelSpec],
    signIn: (Ui, Boolean) => Boolean,
  ): Outcome =
    val labels = presets.map(_.label) ++ CustomServers.map(_.label) :+ ConfigureYourselfLabel :+ NotNowLabel
    ui.choose("Choose a model provider to set up", labels) match
      case None | Some(NotNowLabel) => Outcome.NotNow
      case Some(ConfigureYourselfLabel) => Outcome.ConfigureYourself
      case Some(label) =>
        val ready = CustomServers.find(_.label == label) match
          case Some(server) => custom(ui, server, presets.map(_.name).toSet, keys, list)
          case None => provider(ui, presets.find(_.label == label).get, keys, list, signIn)
        ready match
          case Some(ready) => ready
          case None => run(ui, presets, keys, list, signIn)

  /** A provider at an address the user gives: a name not in `taken`, the address, and a key
    * unless the server takes none, then its model list and a model as for a preset. The key
    * gets a variable of its own (`NAME_API_KEY`, numbered when that is bound already), so it
    * replaces no other key, and no key bound before is sent to the new address. `None` when
    * the user gave up on it. */
  def custom(
    ui: Ui,
    server: CustomServer,
    taken: Set[String],
    keys: KeyBindings,
    list: ModelSpec => List[ModelSpec],
  ): Option[Outcome.Ready] =
    askName(ui, taken).flatMap: name =>
      askAddress(ui, server).flatMap: url =>
        val variable = keyVariable(name, keys)
        val preset = ProviderPreset(name, name, server.api, Some(url), Some(s"$${$variable}"))
        val question =
          s"The API key for $name, saved as $variable in ~/.atc/keys.properties (leave it empty if the server takes none)"
        ui.askSecret(question) match
          case Some(key) => provider(ui, preset, keys, list, (_, _) => false, Some(key))
          case None => provider(ui, preset.copy(key = None), keys, list, (_, _) => false)

  private val ProviderName = "[A-Za-z][A-Za-z0-9._-]*".r

  @tailrec private def askName(ui: Ui, taken: Set[String]): Option[String] =
    ui.askText("A name for it, used in model references such as name/model") match
      case None => None
      case Some(typed) =>
        val name = typed.trim
        if !ProviderName.matches(name) then
          ui.error(s"'$name' cannot be a provider name: start with a letter, then letters, digits, '.', '_' or '-'.")
          askName(ui, taken)
        else if taken.exists(_.equalsIgnoreCase(name)) then
          ui.error(s"The name $name is taken; choose another.")
          askName(ui, taken)
        else Some(name)

  @tailrec private def askAddress(ui: Ui, server: CustomServer): Option[String] =
    ui.askText(s"Its base URL: ${server.address}") match
      case None => None
      case Some(typed) =>
        val url = typed.trim.stripSuffix("/")
        val web =
          try
            val uri = URI(url)
            Option(uri.getScheme).exists(s => s.equalsIgnoreCase("http") || s.equalsIgnoreCase("https")) &&
            Option(uri.getHost).exists(_.nonEmpty)
          catch case _: URISyntaxException => false
        if web then Some(url)
        else
          ui.error(s"'$url' is not an http or https address.")
          askAddress(ui, server)

  /** `NAME_API_KEY` for a provider name, numbered while that variable is bound already. */
  private def keyVariable(name: String, keys: KeyBindings): String =
    val base = name.toUpperCase(Locale.ROOT).map(c => if c.isLetterOrDigit then c else '_') + "_API_KEY"
    (Iterator(base) ++ Iterator.from(2).map(n => s"${base}_$n"))
      .find(v => keys.get(v).isEmpty && !keys.names.contains(v)).get

  /** One provider: a key or a sign-in, its model list, a model. `typed` is a key the user
    * already gave. `None` when the user gave up on it. */
  def provider(
    ui: Ui,
    preset: ProviderPreset,
    keys: KeyBindings,
    list: ModelSpec => List[ModelSpec],
    signIn: (Ui, Boolean) => Boolean,
    typed: Option[String] = None,
  ): Option[Outcome.Ready] =
    val chatgpt = ProviderConfig.canonicalApi(preset.api).contains("chatgpt")
    def ask(): Option[String] =
      preset.keyUrl.foreach(url => ui.info(s"Create a key at $url"))
      ui.askSecret(s"Paste your ${preset.label} API key (saved in ~/.atc/keys.properties, readable only by you)")

    @tailrec def attempt(typed: Option[String]): Option[Outcome.Ready] =
      val bindings = (preset.keyVariable, typed) match
        case (Some(name), Some(key)) => KeyBindings((Path.of("(typed)"), Map(name -> key)) :: keys.files)
        case _ => keys
      ModelCatalog.endpoint(preset.name, preset.config, bindings) match
        case None =>
          ask() match
            case None => None
            case Some(key) => attempt(Some(key))
        case Some(spec) =>
          if typed.isEmpty then preset.keyVariable.foreach(name => ui.info(s"Using $name, which is already set."))
          ui.info(s"Fetching the models of ${preset.label}...")
          val fetched =
            try Right(list(spec))
            catch case NonFatal(e) => Left(reasons(e))
          fetched match
            case Right(Nil) =>
              ui.error(s"${preset.label} lists no models.")
              None
            case Right(models) => chooseModel(ui, models).map(new Outcome.Ready(preset, typed, spec, models, _))
            case Left(why) =>
              ui.error(s"Could not list the models of ${preset.label}: $why")
              // A saved ChatGPT sign-in the backend no longer accepts is replaced by signing in again.
              if chatgpt then
                ui.choose("Sign in to ChatGPT again?", List(SignInAgainLabel, CancelLabel)) match
                  case Some(SignInAgainLabel) if signIn(ui, true) => attempt(typed)
                  case _ => None
              // Without a key there is nothing to correct (a local server that is not running).
              else if preset.keyVariable.isEmpty then None
              else
                ask() match
                  case None => None
                  case Some(key) => attempt(Some(key))

    if chatgpt && !signIn(ui, false) then None else attempt(typed)

  /** Sign in with a ChatGPT plan in the browser, unless already signed in and not `again`. */
  def signIn(ui: Ui, auth: ChatGPTAuth, openBrowser: String => Boolean, again: Boolean): Boolean =
    auth.load().filter(_ => !again) match
      case Some(saved) =>
        ui.info(s"Using your ChatGPT sign-in${saved.email.fold("")(e => s" ($e)")}.")
        true
      case None =>
        try
          Using.resource(auth.begin()): login =>
            ui.info(
              if openBrowser(login.url) then "Sign in to ChatGPT in the browser window that opened, or open:"
              else "Open this address in a browser to sign in to ChatGPT:"
            )
            ui.info(login.url)
            awaitSignIn(ui, login)
        catch
          case NonFatal(e) =>
            ui.error(s"Could not sign in: ${reasons(e)}")
            false

  @tailrec private def awaitSignIn(ui: Ui, login: ChatGPTAuth#Login): Boolean =
    val pasted = ui.askSecret(
      "Press Enter once the browser says you are signed in (or paste the address it ended on if that page did not load)"
    )
    // The browser's callback may still be exchanging its code for tokens.
    val waited = if pasted.isEmpty && login.callbackReceived then Duration.ofSeconds(30) else Duration.ZERO
    pasted.map(login.paste).orElse(login.await(waited)) match
      case Some(Right(tokens)) =>
        ui.info(s"Signed in to ChatGPT${tokens.email.fold("")(e => s" as $e")}.")
        true
      case Some(Left(why)) =>
        ui.error(s"The sign-in failed: $why")
        false
      case None =>
        ui.choose("The browser has not finished signing in", List(KeepWaitingLabel, CancelLabel)) match
          case Some(KeepWaitingLabel) => awaitSignIn(ui, login)
          case _ => false

  private val KeepWaitingLabel = "Keep waiting"
  private val SignInAgainLabel = "Sign in again"
  private val CancelLabel = "Cancel"

  /** A sub-menu: Back returns to where the provider was chosen. */
  private def chooseModel(ui: Ui, models: List[ModelSpec]): Option[ModelSpec] =
    val labels = models.map(m => m.displayName.fold(m.modelId)(name => s"$name  (${m.modelId})"))
    ui.choose("Choose a model", labels :+ Menus.BackLabel).flatMap: label =>
      models.zip(labels).collectFirst { case (m, l) if l == label => m }

  /** The messages of `e` and its causes: an SDK's "Request failed" alone does not say why. */
  /** The messages of `e` and its causes; for a sign-in the backend refused, only why, since
    * the way to sign in again is offered here. */
  private def reasons(e: Throwable): String =
    Debug.causes(e).collectFirst { case s: ChatGPTAuth.SignInNeeded => s.reason }.getOrElse(
      Debug.causes(e).flatMap(t => Option(t.getMessage)).filter(_.nonEmpty).distinct.mkString(": ")
    )
