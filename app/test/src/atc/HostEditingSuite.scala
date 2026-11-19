package atc

import atc.lib.SearchOptions
import atc.perms.*

class HostEditingSuite extends munit.FunSuite:
  test("locked file denials do not suggest an impossible permission request"):
    val env = TestEnv(mkRules =
      root =>
        TestEnv.defaultRules(root) :+
          FileRule(PathPattern("locked", root), Some(Access.None), None, locked = true)
    )
    env.file("locked/note.txt", "unchanged")
    import env.given
    val read = intercept[SecurityException](env.host.read("locked/note.txt"))
    val write = intercept[SecurityException](env.host.write("locked/note.txt", "changed"))
    for error <- List(read, write) do
      assert(error.getMessage.nn.contains("Permission requests cannot widen access"))
      assert(!error.getMessage.nn.contains("Use requestFiles"))
    assertEquals(env.contents("locked/note.txt"), "unchanged")

  test("an edit refuses a file that is not valid UTF-8 instead of rewriting its other bytes"):
    // `read` shows such bytes as U+FFFD; writing that text back would replace them on lines
    // the edit never touched.
    val env = TestEnv()
    import env.given
    val latin1 = Array[Byte]('c', 'a', 'f', 0xe9.toByte, '\n', 'f', 'o', 'o', '\n')
    val path = env.root.resolve("latin.txt").nn
    java.nio.file.Files.write(path, latin1)
    val edits = List[() => Any](
      () => env.host.replaceExact("latin.txt", "foo", "bar"),
      () => env.host.replaceLines("latin.txt", 2, 2, "bar"),
      () => env.host.insertLines("latin.txt", 1, "first"),
      () => env.host.sed("latin.txt", "foo", "bar"),
    )
    for edit <- edits do
      val e = intercept[IllegalArgumentException](edit())
      assert(e.getMessage.nn.contains("not valid UTF-8"), e.getMessage)
      assert(java.util.Arrays.equals(java.nio.file.Files.readAllBytes(path), latin1))
    // Reading stays lenient, and bytes can still be written as bytes.
    assert(env.host.read("latin.txt").contains("foo"))

  test("a byte-order mark stays at the front through line edits"):
    val env = TestEnv()
    import env.given
    val bom = "﻿"
    env.file("bom.txt", bom + "one\r\ntwo\r\n")
    env.host.insertLines("bom.txt", 1, "zero")
    assertEquals(env.contents("bom.txt"), bom + "zero\r\none\r\ntwo\r\n")
    assertEquals(env.host.replaceLines("bom.txt", 1, 1, "first"), "zero")
    assertEquals(env.contents("bom.txt"), bom + "first\r\none\r\ntwo\r\n")
    assertEquals(env.host.sed("bom.txt", "^first", "1st"), 1)
    env.host.replaceExact("bom.txt", "1st", "start")
    assertEquals(env.contents("bom.txt"), bom + "start\r\none\r\ntwo\r\n")

  test("a copied or moved file keeps the permissions of its source"):
    assume(!atc.platform.Platform.isWindows, "POSIX permissions")
    import java.nio.file.Files
    import java.nio.file.attribute.PosixFilePermissions
    val env = TestEnv()
    import env.given
    def mode(rel: String) = PosixFilePermissions.toString(Files.getPosixFilePermissions(env.root.resolve(rel)))
    Files.setPosixFilePermissions(env.file("run.sh", "#!/bin/sh\n"), PosixFilePermissions.fromString("rwx------"))
    env.host.copy("run.sh", "bin/copy.sh")
    assertEquals(mode("bin/copy.sh"), "rwx------")
    env.host.move("run.sh", "bin/run.sh")
    assertEquals(mode("bin/run.sh"), "rwx------")
    // A file that exists keeps its own permissions when it is copied over.
    Files.setPosixFilePermissions(env.file("plain.txt", "old"), PosixFilePermissions.fromString("rw-r--r--"))
    env.host.copy("bin/run.sh", "plain.txt")
    assertEquals(mode("plain.txt"), "rw-r--r--")
    assertEquals(env.contents("plain.txt"), "#!/bin/sh\n")

  test("literal replacement checks uniqueness before writing and preserves literal replacement characters"):
    val env = TestEnv()
    import env.given
    env.file("edit.txt", "first\r\ncost = $1\\n\r\nlast\r\n")
    env.host.replaceExact("edit.txt", "cost = $1\\n", "cost = $2\\t")
    assertEquals(env.contents("edit.txt"), "first\r\ncost = $2\\t\r\nlast\r\n")
    val unchanged = env.contents("edit.txt")
    intercept[IllegalArgumentException](env.host.replaceExact("edit.txt", "absent", "changed"))
    assertEquals(env.contents("edit.txt"), unchanged)
    env.file("edit.txt", "same same")
    intercept[IllegalArgumentException](env.host.replaceExact("edit.txt", "same", "changed"))
    assertEquals(env.contents("edit.txt"), "same same")

  test("range reads stop at the requested window and report work limits"):
    val env = TestEnv()
    import env.given
    env.file("lines.txt", "one\r\ntwo\rthree\nfour")
    assertEquals(env.host.readRange("lines.txt", 2, 3), "two\nthree")
    assertEquals(env.host.readRange("lines.txt", 9, 10), "")
    intercept[IllegalArgumentException](env.host.readRange("lines.txt", 0, 2))
    intercept[IllegalArgumentException](env.host.readRange("lines.txt", 1, 1001))
    env.file("large.txt", "x" * 2100000)
    assert(env.host.readRange("large.txt", 1, 1).contains("read limit reached"))

  test("cat ranges bound output and distinguish read limits from end of file"):
    val env = TestEnv()
    import env.given
    env.file("lines.txt", (1 to 405).map(n => s"line $n").mkString("\n"))
    env.host.cat("lines.txt", 1, Int.MaxValue)
    val preview = env.agentOut.toString
    assert(preview.contains("400\tline 400"))
    assert(!preview.contains("401\tline 401"))
    assert(preview.contains(s"cat(\"lines.txt\", 401, ${Int.MaxValue}) continues"))
    env.clearOutput()
    env.host.cat("lines.txt", 401, Int.MaxValue)
    assert(env.agentOut.toString.contains("405\tline 405"))
    assert(env.agentOut.toString.contains("end of file: 405 lines"))
    env.file("long.txt", "x" * 2100000 + "\nlast\n")
    for from <- List(1, 2) do
      env.clearOutput()
      env.host.cat("long.txt", from, 3)
      assert(env.agentOut.toString.contains("read limit reached"))
      assert(!env.agentOut.toString.contains("end of file"))
      assert(!env.agentOut.toString.contains("nothing to show"))

  test("search enforces match, file, line and character budgets"):
    val env = TestEnv()
    import env.given
    env.file("a.txt", "match\nmatch\n")
    env.file("b.txt", "match\n")
    val first = env.host.search(".", "match", "*.txt", SearchOptions(maxMatches = 1))
    assertEquals(first.matches.map(_.file), List("a.txt"))
    assertEquals(first.filesScanned, 1)
    assert(first.limited)
    val missing = env.host.search(".", "absent", "*.txt", SearchOptions(maxFiles = 1))
    assertEquals(missing.filesScanned, 1)
    assert(missing.limited)
    val lines = env.host.search(".", "match", "a.txt", SearchOptions(maxLinesPerFile = 1))
    assertEquals(lines.matches.size, 1)
    assert(lines.limited)
    val chars = env.host.search(".", "match", "a.txt", SearchOptions(maxCharsPerFile = 2))
    assertEquals(chars.matches, Nil)
    assert(chars.limited)
    val complete = env.host.search(".", "match", "*.txt", SearchOptions())
    assertEquals(complete.matches.size, 3)
    assert(!complete.limited)
    // A budget reached exactly at the end of the input cut nothing.
    val exactChars = SearchOptions(maxCharsPerFile = env.contents("a.txt").length)
    for exact <- List(
        SearchOptions(maxMatches = 3),
        SearchOptions(maxFiles = 2),
        SearchOptions(maxLinesPerFile = 2),
        exactChars,
      )
    do
      val result = env.host.search(".", "match", "*.txt", exact)
      assertEquals(result.matches.size, 3, exact.toString)
      assert(!result.limited, exact.toString)
    // One character less is a real cut.
    val cut = env.host.search(".", "match", "a.txt", SearchOptions(maxCharsPerFile = exactChars.maxCharsPerFile - 1))
    assert(cut.limited)

  test("search and change previews respect classified paths"):
    val env = TestEnv(TestEnv.withSecrets)
    import env.given
    env.file("public.txt", "before")
    env.file("secrets/key.txt", "PRIVATE_VALUE")
    env.host.write("public.txt", "after")
    assert(env.fileChanges.head.preview.contains("- before"))
    assert(env.fileChanges.head.preview.contains("+ after"))
    assertEquals(env.host.search(".", "PRIVATE", "*.txt", SearchOptions()).matches, Nil)
    intercept[SecurityException](env.host.replaceExact("secrets/key.txt", "PRIVATE_VALUE", "changed"))
    env.host.writeClassified("secrets/key.txt", env.host.classify("OTHER_PRIVATE_VALUE"))
    assertEquals(env.fileChanges.size, 1)
