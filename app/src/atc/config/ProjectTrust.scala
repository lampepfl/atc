package atc.config

import atc.Debug
import atc.platform.PlatformPath

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.HexFormat

/** Whether the user trusts what a project's configuration adds beyond its own files: the
  * commands and hosts its `.atc/config.json` pre-approves, the classified model it names and
  * the bindings of its `.atc/keys.properties`. A cloned repository can ship both files, so
  * [[Config.load]] leaves these out until the user has trusted them. Trust is a fingerprint of
  * exactly these grants, kept per project in `trusted-projects.json` beside the global config,
  * so a change to them (a `git pull`, say) is put to the user again and other edits are not. */
object ProjectTrust:
  val StoreFile = "trusted-projects.json"

  /** The grants of one project that need trust. `keys` are the names its key file binds. */
  final case class Grants(
    commands: List[String],
    hosts: List[String],
    classifiedModel: Option[String],
    keys: List[String],
    fingerprint: String,
  ):
    def isEmpty: Boolean = commands.isEmpty && hosts.isEmpty && classifiedModel.isEmpty && keys.isEmpty

    /** What the user is asked to trust, one row per kind, with every entry. */
    def describe: List[String] =
      List(
        Option.when(commands.nonEmpty)(s"pre-approves the commands ${commands.mkString(", ")}"),
        Option.when(hosts.nonEmpty)(s"allows the hosts ${hosts.mkString(", ")}"),
        classifiedModel.map(model => s"sends classified data to the model $model"),
        Option.when(keys.nonEmpty)(s"binds the keys ${keys.mkString(", ")} in .atc/keys.properties"),
      ).flatten

  /** Grants of the project at `root` the user has not trusted, and whether an earlier
    * version of them was trusted. */
  final case class Pending(root: Path, grants: Grants, changed: Boolean)

  /** The untrusted grants of the project `cwd` belongs to, with the store in `atcDir`; `None`
    * when it has none or they are trusted. */
  def pending(cwd: Path, atcDir: Path): Option[Pending] =
    Config.projectRoot(cwd).flatMap(root => pending(root, readConfig(root), readKeys(root), atcDir))

  /** As [[pending]], for the project at `root` whose config reads as `config` and whose key file
    * binds `keys`. In the home directory the "project" is the global config itself, which is the
    * user's own. */
  private[config] def pending(
    root: Path,
    config: ujson.Obj,
    keys: Map[String, String],
    atcDir: Path,
  ): Option[Pending] =
    if Config.samePath(root.resolve(".atc").nn, atcDir) then None
    else
      val grants = of(config, keys)
      val recorded = read(atcDir).get(key(root))
      Option.when(!grants.isEmpty && !recorded.contains(grants.fingerprint))(
        Pending(root, grants, changed = recorded.isDefined)
      )

  /** Trust what the project `cwd` belongs to grants now. */
  def trust(cwd: Path, atcDir: Path): Unit =
    Config.projectRoot(cwd).foreach(root => record(root, of(readConfig(root), readKeys(root)).fingerprint, atcDir))

  /** Trust the grants `pending` describes, and not a later version of the files. */
  def trust(pending: Pending, atcDir: Path): Unit = record(pending.root, pending.grants.fingerprint, atcDir)

  /** Write the starter project config in `dir` ([[Config.initProject]]) and trust it, since ATC
    * wrote what it grants. A key file that `dir` already had and the user has not trusted keeps
    * the project waiting for the prompt that shows its bindings. Returns the paths written. */
  def initProject(dir: Path, atcDir: Path): List[Path] =
    val root = dir.toAbsolutePath.nn.normalize.nn
    val waiting = pending(root, atcDir).exists(_.root == root)
    val created = Config.initProject(root, atcDir.resolve("config.json").nn)
    if created.nonEmpty && !waiting then trust(root, atcDir)
    created

  private def record(root: Path, fingerprint: String, atcDir: Path): Unit =
    val store = read(atcDir).updated(key(root), fingerprint)
    val text = ujson.write(ujson.Obj.from(store.toList.sortBy(_._1).map((k, v) => k -> ujson.Str(v))), indent = 2)
    Config.replaceFile(atcDir.resolve(StoreFile).nn, text + "\n", keepPermissions = false)

  /** `layer` without the settings that need trust. */
  def withoutGrants(layer: ConfigLayer): ConfigLayer =
    val json = ujson.Obj.from(layer.json.value.filterNot((k, v) => Removed(k) && !v.isNull))
    layer.copy(json = json, config = ConfigLayer.settings(json, layer.path.fold("")(_.toString)))

  private val Removed = Set("commands", "hosts", "classifiedModel")

  private def readConfig(root: Path): ujson.Obj =
    val config = Config.projectPath(root)
    if Files.isRegularFile(config) then ObjectText.parse(Files.readString(config).nn, config.toString)
    else ujson.Obj()

  private def readKeys(root: Path): Map[String, String] =
    val keyFile = Config.keysPath(root)
    if Files.isRegularFile(keyFile) then KeyBindings.read(keyFile) else Map.empty

  private def of(json: ujson.Obj, bindings: Map[String, String]): Grants =
    def strings(key: String) = json.value.get(key).flatMap(_.arrOpt).fold(Nil)(_.toList.flatMap(_.strOpt))
    val keys = bindings.toList.sortBy(_._1)
    val classified = json.value.get("classifiedModel").flatMap(_.strOpt)
    val granted = ujson.Obj(
      "commands" -> ujson.Arr.from(strings("commands")),
      "hosts" -> ujson.Arr.from(strings("hosts")),
      "classifiedModel" -> classified.fold[ujson.Value](ujson.Null)(ujson.Str(_)),
      "keys" -> ujson.Obj.from(keys.map((name, value) => name -> ujson.Str(value))),
    )
    val digest = MessageDigest.getInstance("SHA-256").nn.digest(ujson.write(granted).getBytes(UTF_8))
    Grants(strings("commands"), strings("hosts"), classified, keys.map(_._1), HexFormat.of().nn.formatHex(digest))

  private def key(root: Path): String = PlatformPath.portable(PlatformPath.canonical(root))

  /** The recorded fingerprints; an unreadable store trusts nothing. */
  private def read(atcDir: Path): Map[String, String] =
    val file = atcDir.resolve(StoreFile).nn
    try
      if !Files.isRegularFile(file) then Map.empty
      else ujson.read(Files.readString(file).nn).obj.toMap.collect { case (k, ujson.Str(v)) => k -> v }
    catch
      case e: Exception =>
        Debug.log(s"ignoring $file: ${Debug.message(e)}")
        Map.empty
