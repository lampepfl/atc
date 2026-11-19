package atc.platform

import java.io.IOException
import java.nio.channels.{Channels, FileChannel}
import java.nio.file.{AtomicMoveNotSupportedException, FileSystems, Files, LinkOption, OpenOption, Path}
import java.nio.file.StandardCopyOption
import java.nio.file.StandardOpenOption.{CREATE_NEW, WRITE}
import java.nio.file.attribute.{PosixFilePermissions, UserPrincipal}
import java.util.UUID
import scala.util.Using

/** Files that can hold secrets: API keys, tokens, prompts and saved sessions. A file these
  * methods create is readable and writable by its owner only where the file system has POSIX
  * permissions, and gets the file system's default access elsewhere. */
private[atc] object OwnerOnlyFiles:
  private val OwnerOnly = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))

  /** The owner of `path` itself, a link not followed, when that is neither the user running
    * ATC nor the system's administrator, who could do anything here already. `None` also
    * where the owner cannot be told, so that a file system which records none does not make
    * every file another user's. */
  def foreignOwner(path: Path): Option[String] =
    try
      val owner = Files.getOwner(path, LinkOption.NOFOLLOW_LINKS).nn
      val name = owner.getName.nn
      Option.when(currentUser.exists(_ != owner) && !Administrators.contains(name))(name)
    catch case _: IOException | _: UnsupportedOperationException => None

  private lazy val currentUser: Option[UserPrincipal] =
    try
      val lookup = FileSystems.getDefault.nn.getUserPrincipalLookupService.nn
      Option(lookup.lookupPrincipalByName(System.getProperty("user.name")))
    catch case _: Exception => None

  /** As the JDK names them; a Windows in another language names its groups otherwise.
    * TrustedInstaller, the service that installs Windows, owns the system drive's root. */
  private val Administrators =
    Set("root", "BUILTIN\\Administrators", "NT AUTHORITY\\SYSTEM", "NT SERVICE\\TrustedInstaller")

  /** Open `path` with `options`; a file the call creates is owner-only. */
  def open(path: Path, options: OpenOption*): FileChannel =
    val set = java.util.Set.of(options*)
    try FileChannel.open(path, set, OwnerOnly).nn
    catch case _: UnsupportedOperationException => FileChannel.open(path, set).nn

  /** Create `path` holding `content`; throws `FileAlreadyExistsException` when it exists. */
  def create(path: Path, content: Array[Byte]): Unit =
    Using.resource(Channels.newOutputStream(open(path, CREATE_NEW, WRITE)).nn)(_.write(content))

  /** Replace `target` with `content` through a temporary file beside it, moved into place
    * atomically where the file system allows. The new file is owner-only, or with
    * `keepPermissions` has the POSIX permissions `target` had. A failure leaves `target` as
    * it was and removes the temporary file. */
  def replace(target: Path, content: Array[Byte], keepPermissions: Boolean): Unit =
    val dir = target.toAbsolutePath.nn.getParent.nn
    Files.createDirectories(dir)
    val temp = dir.resolve(s".${target.getFileName}.${UUID.randomUUID()}.tmp").nn
    try
      create(temp, content)
      if keepPermissions then
        try Files.setPosixFilePermissions(temp, Files.getPosixFilePermissions(target))
        catch case _: UnsupportedOperationException => ()
      try Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch case _: AtomicMoveNotSupportedException => Files.move(temp, target, StandardCopyOption.REPLACE_EXISTING)
    finally Files.deleteIfExists(temp)
