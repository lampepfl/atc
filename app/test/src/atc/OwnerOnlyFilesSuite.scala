package atc

import atc.platform.{OwnerOnlyFiles, Platform}

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileAlreadyExistsException, Files, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions

class OwnerOnlyFilesSuite extends munit.FunSuite:
  private def permissions(path: Path): String = PosixFilePermissions.toString(Files.getPosixFilePermissions(path))
  private def utf8(text: String): Array[Byte] = text.getBytes(UTF_8)

  /** Run `body` in a new directory, then delete the entries `created` and the directory. */
  private def inTempDir(created: String*)(body: Path => Unit): Unit =
    val dir = Files.createTempDirectory("atc-owner-only").nn
    try body(dir)
    finally
      created.foreach(name => Files.deleteIfExists(dir.resolve(name)))
      Files.deleteIfExists(dir)

  test("create and open make owner-only files, and create never overwrites"):
    inTempDir("secret", "lock"): dir =>
      val secret = dir.resolve("secret").nn
      OwnerOnlyFiles.create(secret, utf8("KEY=é\n"))
      intercept[FileAlreadyExistsException](OwnerOnlyFiles.create(secret, utf8("other")))
      assertEquals(Files.readString(secret), "KEY=é\n")
      val lock = dir.resolve("lock").nn
      OwnerOnlyFiles.open(lock, StandardOpenOption.CREATE, StandardOpenOption.WRITE).close()
      if !Platform.isWindows then
        assertEquals(permissions(secret), "rw-------")
        assertEquals(permissions(lock), "rw-------")

  test("replace is owner-only or keeps the old permissions, and a failure leaves no temporary file"):
    inTempDir("config", "blocked/child", "blocked"): dir =>
      val config = dir.resolve("config").nn
      OwnerOnlyFiles.replace(config, utf8("new"), keepPermissions = false)
      assertEquals(Files.readString(config), "new")
      if !Platform.isWindows then
        assertEquals(permissions(config), "rw-------")
        Files.setPosixFilePermissions(config, PosixFilePermissions.fromString("rw-r--r--"))
      OwnerOnlyFiles.replace(config, utf8("kept"), keepPermissions = true)
      assertEquals(Files.readString(config), "kept")
      if !Platform.isWindows then assertEquals(permissions(config), "rw-r--r--")
      OwnerOnlyFiles.replace(config, utf8("private"), keepPermissions = false)
      if !Platform.isWindows then assertEquals(permissions(config), "rw-------")
      val blocked = Files.createDirectories(dir.resolve("blocked/child")).nn.getParent.nn
      intercept[IOException](OwnerOnlyFiles.replace(blocked, utf8("lost"), keepPermissions = false))
      val entries = Files.list(dir).nn
      try assertEquals(entries.count(), 2L)
      finally entries.close()
