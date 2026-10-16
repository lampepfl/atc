package atc.llm

import atc.Debug
import atc.config.{ModelConfig, ModelSpec}

import java.util.Locale
import java.util.concurrent.atomic.AtomicBoolean
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/** What every provider adapter takes from its [[ModelSpec]]: the names, the per-model
  * settings, and the context window and output limit the agent loop reads. */
private[llm] abstract class SpecModel(val spec: ModelSpec) extends ChatModel:
  val alias: String = spec.alias
  override val ref: String = spec.ref
  val modelId: String = spec.modelId
  protected val settings: ModelConfig = spec.settings
  override def contextWindow: Option[Int] = settings.contextWindow.map(_.toInt)
  override val maxOutputTokens: Option[Int] = settings.maxTokens
  override val efforts: List[String] = settings.efforts.getOrElse(knownEfforts).map(_.toLowerCase(Locale.ROOT))
  override val defaultEffort: Option[String] = settings.reasoning.map(_.toLowerCase(Locale.ROOT))
  effort = defaultEffort

  /** Every effort the provider's api accepts, for a model whose config lists none. */
  protected def knownEfforts: List[String]

  /** The models the provider's endpoint lists, each as a spec named by its model id. Called on
    * a model whose spec has an empty alias and model id ([[ChatModel.listModels]]). */
  private[atc] def listModels(): List[ModelSpec]

  @volatile private var webSearchOn = settings.webSearch.getOrElse(false)
  /** Set once the provider rejected its web search tool for this model. */
  @volatile private var webSearchRejected = false
  def webSearch: Boolean = webSearchOn && !webSearchRejected
  override def useWebSearch(on: Boolean): Unit = webSearchOn = on

  /** Run one streaming request. Web search is best effort: when the provider
    * rejects the tool before anything was streamed, this model continues
    * without it and the request is sent again. */
  protected def withWebSearchFallback(sink: StreamSink)(request: StreamSink => Completion): Completion =
    if !webSearch then request(sink)
    else
      val streamed = AtomicBoolean(false)
      val tracking = StreamSink(
        t => { streamed.set(true); sink.text(t) },
        n => { streamed.set(true); sink.note(n) },
        d => { streamed.set(true); sink.thinking(d) },
      )
      try request(tracking)
      catch
        case NonFatal(e) if !streamed.get() && Providers.isWebSearchRejection(e) =>
          webSearchRejected = true
          Debug.log(s"$ref: the provider rejected web search; continuing without it (${Debug.message(e)})")
          request(sink)

  /** The provider-native form of an assistant turn this model produced, to replay
    * as it was; `None` for a turn of another model or protocol. */
  protected def replay[P: ClassTag](turn: Option[NativeTurn]): Option[P] =
    turn.filter(_.isFor(providerKey, ref)).map(_.payload).collect { case p: P => p }
