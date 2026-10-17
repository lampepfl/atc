package atc

import atc.host.*
import atc.lib.{Todo, TodoStatus}
import atc.perms.*
import atc.platform.{PathGlob, Platform, PlatformPath}

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{Files, LinkOption, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.collection.mutable.ListBuffer

/** The host's `Interface` implementation under the policy, called directly. */
class HostSuite extends munit.FunSuite:
  private val echoCommand = ProcessFixture.command("echo")
  private val echoPattern = ProcessFixture.pattern("echo")

  private val env = TestEnv(
    root =>
      List(
        FileRule(PathPattern(".", root), Some(Access.Write), None),
        FileRule(PathPattern("secrets", root), None, Some(true)),
        FileRule(PathPattern("./private", root), Some(Access.None), None),
      ),
    commands = List(echoPattern),
    hosts = List("example.com"),
    prefix = "atc-host",
  )
  private val root = env.root
  env.file("src/A.scala", "object A")
  env.file("secrets/key.txt", "s3cret")
  env.file("secrets/sub/deep.txt", "deep")
  env.file("private/p.txt", "p")
  env.file("README.md", "hello")
  import env.given
  import env.host.*

  override def beforeEach(context: BeforeEach): Unit =
    env.clearOutput()
    env.decisions = Nil

  test("plain read/write within cwd, relative paths"):
    assertEquals(read("src/A.scala"), "object A")
    write("src/B.scala", "object B")
    assertEquals(Files.readString(root.resolve("src/B.scala")), "object B")
    append("src/B.scala", "\n// more")
    assert(Files.readString(root.resolve("src/B.scala")).nn.endsWith("// more"))
    assert(exists("README.md"))
    assertEquals(access("/a/b/c.txt").name, "c.txt")

  test("sed with quote/quoteReplacement makes a literal edit, metacharacters and all"):
    Files.writeString(root.resolve("edit.txt"), "f(a.b) = $x\nf(a-b) = $x\nf(a.b) = $x\n")
    assertEquals(sed("edit.txt", quote("f(a.b) = $x"), quoteReplacement("g[a.b] = \\1 $y")), 2)
    assertEquals(
      Files.readString(root.resolve("edit.txt")),
      "g[a.b] = \\1 $y\nf(a-b) = $x\ng[a.b] = \\1 $y\n",
    )

  test("sed makes a regex edit with per-line anchors and reports how many matches it changed"):
    Files.writeString(root.resolve("sed.txt"), "x = 1\ny = 2\nz = 3\n")
    assertEquals(sed("sed.txt", """^(\w) = (\d)$""", "$1 := $2"), 3)
    assertEquals(Files.readString(root.resolve("sed.txt")), "x := 1\ny := 2\nz := 3\n")
    // a pattern with an explicit newline may span lines; `.` never does
    assertEquals(sed("sed.txt", ":= 1\ny", ":= 10\ny"), 1)
    assertEquals(Files.readString(root.resolve("sed.txt")), "x := 10\ny := 2\nz := 3\n")
    intercept[IllegalArgumentException](sed("sed.txt", "1.y", "1y"))

  test("sed accepts sed-style \\1, \\n, \\t and \\$ in the replacement"):
    Files.writeString(root.resolve("sed2.txt"), "a=1\nb=2\n")
    assertEquals(sed("sed2.txt", """(\w)=(\d)""", """\2:\1\t\$\n"""), 2)
    assertEquals(Files.readString(root.resolve("sed2.txt")), "1:a\t$\n\n2:b\t$\n\n")
    // a backslash before anything else escapes it (Java), and a trailing one is literal
    Files.writeString(root.resolve("sed2.txt"), "a=1\n")
    assertEquals(sed("sed2.txt", "=", """\=\"""), 1)
    assertEquals(Files.readString(root.resolve("sed2.txt")), "a=\\1\n")

  test("sed refuses a no-op, an empty pattern and a malformed regex, leaving the file alone"):
    Files.writeString(root.resolve("sed3.txt"), "content")
    val e = intercept[IllegalArgumentException](sed("sed3.txt", "absent.*", "x"))
    assert(e.getMessage.nn.contains("matches nothing"), e.getMessage)
    intercept[IllegalArgumentException](sed("sed3.txt", "", "x"))
    intercept[IllegalArgumentException](sed("sed3.txt", "(unclosed", "x"))
    assertEquals(Files.readString(root.resolve("sed3.txt")), "content")

  test("cat prints cat -n numbered lines, capped with a note on how to see the rest"):
    Files.writeString(root.resolve("cat.txt"), "one\ntwo\nthree\n")
    cat("cat.txt")
    assertEquals(env.agentOut.toString, "     1\tone\n     2\ttwo\n     3\tthree\n")
    assertEquals(env.userOut.toString, env.agentOut.toString) // the user sees the same text
    env.agentOut.clear()
    Files.writeString(root.resolve("big.txt"), (1 to 1000).map(i => s"line $i").mkString("\n"))
    cat("big.txt")
    val out = env.agentOut.toString
    assert(out.startsWith("     1\tline 1\n"), out.take(40))
    assert(out.contains(f"${Host.CatMaxLines}%6d\tline ${Host.CatMaxLines}\n"), out.takeRight(200))
    assert(!out.contains(s"line ${Host.CatMaxLines + 1}\n"), out.takeRight(200))
    assert(
      out.endsWith(
        s"""... [600 more lines (1000 in all): cat("big.txt", ${Host.CatMaxLines + 1}, ${2 *
            Host.CatMaxLines}) shows the next]\n"""
      ),
      out.takeRight(200),
    )
    env.agentOut.clear()
    Files.writeString(root.resolve("empty.txt"), "")
    cat("empty.txt")
    assertEquals(env.agentOut.toString, "[empty file]\n")

  test("cat(path, from, to) prints a 1-based inclusive window and marks the end of the file"):
    Files.writeString(root.resolve("win.txt"), (1 to 12).map(i => s"l$i").mkString("\n"))
    cat("win.txt", 10, 11)
    assertEquals(env.agentOut.toString, "    10\tl10\n    11\tl11\n")
    env.agentOut.clear()
    cat("win.txt", 12, 20) // runs past the end: what exists, then the marker
    assertEquals(env.agentOut.toString, "    12\tl12\n[end of file: 12 lines]\n")
    env.agentOut.clear()
    cat("win.txt", 13, 20)
    assertEquals(env.agentOut.toString, "[nothing to show: win.txt has 12 lines]\n")
    env.agentOut.clear()
    intercept[IllegalArgumentException](cat("win.txt", 0, 3))
    intercept[IllegalArgumentException](cat("win.txt", 5, 4))
    assertEquals(env.agentOut.toString, "")
    // very long lines (minified files) are cut with a marker instead of flooding the result
    Files.writeString(root.resolve("long.txt"), "x" * (Host.CatMaxLineChars + 7) + "\nshort\n")
    cat("long.txt", 1, 2)
    assertEquals(env.agentOut.toString, "     1\t" + "x" * Host.CatMaxLineChars + " ... [+7 chars]\n     2\tshort\n")
    assertEquals(readRange("long.txt", 1, 2), "x" * Host.CatMaxLineChars + " ... [+7 chars]\nshort")

  test("cat retains only a capped prefix of a giant line and preserves CR/LF line semantics"):
    val giantChars = 2 * 1024 * 1024 + 17
    val chunk = "x" * 8192
    val writer = Files.newBufferedWriter(root.resolve("giant-line.txt"))
    try
      var remaining = giantChars
      while remaining > 0 do
        val count = math.min(remaining, chunk.length)
        writer.write(chunk, 0, count)
        remaining -= count
      writer.write("\r\nshort\rthird\nlast\n")
    finally writer.close()
    cat("giant-line.txt")
    assertEquals(
      env.agentOut.toString,
      "     1\t" + "x" * Host.CatMaxLineChars +
        s" ... [+${giantChars - Host.CatMaxLineChars} chars]\n" +
        "     2\tshort\n     3\tthird\n     4\tlast\n",
    )

  test("listings and grep hits are relative to the working directory, absolute outside it"):
    assert(ls(".").contains("src"), ls(".").toString)
    assert(walk(".").contains("src/A.scala"), walk(".").toString)
    assert(find(".", "*.scala").contains("src/A.scala"))
    assertEquals(grepRecursive("src", "object A").map(_.file), List("src/A.scala"))
    val outside = TestEnv.outsideDir()
    env.decisions = List(Decision.AllowSession)
    requestFiles(outside.toString, atc.lib.Access.Read, "look outside") {
      assertEquals(
        ls(outside.toString),
        List(PlatformPath.portable(outside.resolve("o.txt")))
      ) // absolute: not under cwd
    }

  test("find matches the name for a plain glob and the relative path for one with / or **"):
    Files.createDirectories(root.resolve("g/deep/er"))
    Files.writeString(root.resolve("g/top.scala"), "")
    Files.writeString(root.resolve("g/deep/mid.scala"), "")
    Files.writeString(root.resolve("g/deep/er/low.scala"), "")
    Files.writeString(root.resolve("g/deep/er/note.txt"), "note")
    assertEquals(find("g", "*.scala").sorted, List("g/deep/er/low.scala", "g/deep/mid.scala", "g/top.scala"))
    assertEquals(find("g", "**/*.scala").sorted, List("g/deep/er/low.scala", "g/deep/mid.scala", "g/top.scala"))
    assertEquals(find("g", "deep/**/*.scala").sorted, List("g/deep/er/low.scala", "g/deep/mid.scala"))
    assertEquals(find("g", "deep/*.scala"), List("g/deep/mid.scala"))
    assertEquals(find("g", "**/er/*.{scala,txt}").sorted, List("g/deep/er/low.scala", "g/deep/er/note.txt"))
    assertEquals(find(".", "g/**").sorted, walk("g").filter(p => p.endsWith(".scala") || p.endsWith(".txt")).sorted)
    assertEquals(grepRecursive("g", "note", "**/er/*.txt").map(_.file), List("g/deep/er/note.txt"))

  test("move and copy files; classified and directory sources are refused"):
    Files.writeString(root.resolve("mv.txt"), "payload")
    move("mv.txt", "moved/mv2.txt")
    assert(!Files.exists(root.resolve("mv.txt")))
    assertEquals(Files.readString(root.resolve("moved/mv2.txt")), "payload")
    copy("moved/mv2.txt", "copy.txt")
    assertEquals(Files.readString(root.resolve("copy.txt")), "payload")
    assert(Files.exists(root.resolve("moved/mv2.txt")))
    intercept[IllegalArgumentException](move("moved", "elsewhere"))
    val e = intercept[SecurityException](move("secrets/key.txt", "leaked.txt")) // the read refuses classified
    assert(e.getMessage.nn.contains("readClassified"), e.getMessage)
    assert(Files.exists(root.resolve("secrets/key.txt")) && !Files.exists(root.resolve("leaked.txt")))
    intercept[SecurityException](copy("README.md", "secrets/copy.txt")) // the write refuses a classified target

  test("move checks source write permission before creating or replacing the destination"):
    val restricted = TestEnv(root =>
      List(
        FileRule(PathPattern(".", root), Some(Access.Write), None),
        FileRule(PathPattern("move-read-only.txt", root), Some(Access.Read), None),
      )
    )
    restricted.file("move-read-only.txt", "source content")
    restricted.file("move-existing.txt", "original destination")
    for destination <- List("move-existing.txt", "move-new-dir/file.txt") do
      intercept[SecurityException](restricted.host.move("move-read-only.txt", destination)(using restricted.fs))
    assertEquals(restricted.contents("move-read-only.txt"), "source content")
    assertEquals(restricted.contents("move-existing.txt"), "original destination")
    assert(!restricted.existsOnDisk("move-new-dir"))

  test("self-copy preserves content but still checks source and target permissions; exact self-move stays a no-op"):
    Files.writeString(root.resolve("self-copy.txt"), "payload")
    copy("self-copy.txt", "./self-copy.txt")
    assertEquals(Files.readString(root.resolve("self-copy.txt")), "payload")

    val secretBefore = Files.readString(root.resolve("secrets/key.txt"))
    intercept[SecurityException](copy("secrets/key.txt", "secrets/./key.txt"))
    assertEquals(Files.readString(root.resolve("secrets/key.txt")), secretBefore)
    move("secrets/key.txt", "secrets/./key.txt")
    assertEquals(Files.readString(root.resolve("secrets/key.txt")), secretBefore)

    val readOnly = TestEnv(root => List(FileRule(PathPattern(".", root), Some(Access.Read), None)))
    readOnly.file("same.txt", "read only")
    intercept[SecurityException](readOnly.host.copy("same.txt", "same.txt")(using readOnly.fs))
    assertEquals(readOnly.contents("same.txt"), "read only")

  test("copy and move preserve same-inode hard links and enforce alias permissions"):
    val original = root.resolve("hard-original.txt")
    Files.writeString(original, "hard-linked payload")
    val copyAlias = root.resolve("hard-copy.txt")
    val hardLinksSupported =
      try
        Files.createLink(copyAlias, original)
        true
      catch
        case _: UnsupportedOperationException | _: IOException | _: SecurityException => false
    if !hardLinksSupported then Files.deleteIfExists(original)
    assume(hardLinksSupported, "hard links are not supported on this filesystem")

    copy("hard-original.txt", "hard-copy.txt")
    assertEquals(Files.readString(original), "hard-linked payload")
    assertEquals(Files.readString(copyAlias), "hard-linked payload")

    val moveSource = root.resolve("hard-move-source.txt")
    val moveTarget = root.resolve("hard-move-target.txt")
    Files.writeString(moveSource, "move payload")
    Files.createLink(moveTarget, moveSource)
    move("hard-move-source.txt", "hard-move-target.txt")
    assert(!Files.exists(moveSource))
    assertEquals(Files.readString(moveTarget), "move payload")

    val classifiedSource = root.resolve("secrets/hard-source.txt")
    Files.createLink(classifiedSource, original)
    intercept[SecurityException](move("secrets/hard-source.txt", "hard-original.txt"))
    assert(Files.exists(classifiedSource))
    assertEquals(Files.readString(original), "hard-linked payload")

    val classifiedTarget = root.resolve("secrets/hard-target.txt")
    Files.createLink(classifiedTarget, original)
    intercept[SecurityException](copy("hard-original.txt", "secrets/hard-target.txt"))
    assertEquals(Files.readString(original), "hard-linked payload")

    val deniedTarget = root.resolve("private/hard-target.txt")
    Files.createLink(deniedTarget, original)
    intercept[SecurityException](copy("hard-original.txt", "private/hard-target.txt"))
    assertEquals(Files.readString(original), "hard-linked payload")

    val alias = TestEnv(root =>
      List(
        FileRule(PathPattern("hard-read-only-source.txt", root), Some(Access.Read), None),
        FileRule(PathPattern("hard-writable-target.txt", root), Some(Access.Write), None),
      )
    )
    val readOnlySource = alias.file("hard-read-only-source.txt", "guarded move")
    val writableTarget = alias.root.resolve("hard-writable-target.txt")
    Files.createLink(writableTarget, readOnlySource)
    intercept[SecurityException](
      alias.host.move("hard-read-only-source.txt", "hard-writable-target.txt")(using alias.fs)
    )
    assertEquals(Files.readString(readOnlySource), "guarded move")
    assertEquals(Files.readString(writableTarget), "guarded move")

    List(classifiedSource, classifiedTarget, deniedTarget, copyAlias, moveTarget, original)
      .foreach(Files.deleteIfExists(_))

  test("replaceLines/insertLines edit by cat's line numbers, return the old text, keep the newline style"):
    Files.writeString(root.resolve("lines.txt"), "one\ntwo\nthree\nfour\n")
    assertEquals(replaceLines("lines.txt", 2, 3, "TWO\nTHREE\nextra"), "two\nthree")
    assertEquals(Files.readString(root.resolve("lines.txt")), "one\nTWO\nTHREE\nextra\nfour\n")
    assertEquals(replaceLines("lines.txt", 4, 4, ""), "extra") // empty text deletes
    assertEquals(Files.readString(root.resolve("lines.txt")), "one\nTWO\nTHREE\nfour\n")
    insertLines("lines.txt", 1, "zero")
    insertLines("lines.txt", 6, "five\n") // lineCount + 1 appends; one trailing newline is not an extra line
    assertEquals(Files.readString(root.resolve("lines.txt")), "zero\none\nTWO\nTHREE\nfour\nfive\n")
    val e = intercept[IllegalArgumentException](replaceLines("lines.txt", 5, 9, "x"))
    assert(e.getMessage.nn.contains("has 6 lines"), e.getMessage)
    intercept[IllegalArgumentException](insertLines("lines.txt", 8, "x"))
    intercept[IllegalArgumentException](replaceLines("lines.txt", 0, 1, "x"))
    Files.writeString(root.resolve("crlf.txt"), "a\r\nb\r\n")
    assertEquals(replaceLines("crlf.txt", 2, 2, "B"), "b")
    assertEquals(Files.readString(root.resolve("crlf.txt")), "a\r\nB\r\n")
    Files.writeString(root.resolve("nonl.txt"), "a\nb")
    replaceLines("nonl.txt", 1, 1, "A")
    assertEquals(Files.readString(root.resolve("nonl.txt")), "A\nb") // no trailing newline stays that way
    Files.writeString(root.resolve("mixed.txt"), "a\r\nb\nc\rd")
    assertEquals(replaceLines("mixed.txt", 2, 3, "B\r\nC\n"), "b\r\nc")
    assertEquals(Files.readString(root.resolve("mixed.txt")), "a\r\nB\r\nC\r\nd")
    Files.writeString(root.resolve("bare-cr.txt"), "a\rb\r")
    insertLines("bare-cr.txt", 2, "middle\r\n")
    assertEquals(Files.readString(root.resolve("bare-cr.txt")), "a\rmiddle\rb\r")
    Files.writeString(root.resolve("empty2.txt"), "")
    insertLines("empty2.txt", 1, "first")
    assertEquals(Files.readString(root.resolve("empty2.txt")), "first\n")

  test("parseCommandLine, parsePipeline and PathGlob (pure helpers)"):
    val escapedTail = if Platform.isWindows then List("e\\", "f") else List("e f")
    assertEquals(
      CommandLine.parseCommandLine("""git commit -m 'a b' --x="c d" e\ f"""),
      List("git", "commit", "-m", "a b", "--x=c d") ++ escapedTail
    )
    intercept[IllegalArgumentException](CommandLine.parseCommandLine("a > b")) // one program only
    intercept[IllegalArgumentException](CommandLine.parseCommandLine("a | b"))
    val p = CommandLine.parsePipeline("cat < in.txt | sort -u 2>&1 | head -3 >> out.txt")
    assertEquals(p.stages.map(_.argv), List(List("cat"), List("sort", "-u"), List("head", "-3")))
    assertEquals(p.stages.map(_.mergeErr), List(false, true, false))
    assertEquals((p.stdinFile, p.stdoutFile, p.append), (Some("in.txt"), Some("out.txt"), true))
    assertEquals(p.line, "cat < in.txt | sort -u 2>&1 | head -3 >> out.txt")
    assertEquals(CommandLine.parsePipeline("cat < 'in file' > 'a b'").line, """cat < "in file" > "a b"""")
    val q = CommandLine.parsePipeline("echo 'a | b' >out.txt")
    assertEquals((q.stages.head.argv, q.stdoutFile, q.append), (List("echo", "a | b"), Some("out.txt"), false))
    assertEquals(
      CommandLine.parsePipeline("printf 2 > two.txt").stages.head.argv,
      List("printf", "2")
    ) // a bare 2 before > is a word...
    for bad <- List(
        "a && b",
        "a; b",
        "a || b",
        "a &",
        "a 2> err",
        "a &> all",
        "a > ",
        "a | ",
        "| a",
        "$(x)",
        "a `b`",
        "a << EOF",
        "",
        "a < x < y",
        "a > x > y",
        "a > x | b", // a redirection belongs to the stage it is written on
        "a | b < x"
      )
    do
      intercept[IllegalArgumentException](CommandLine.parsePipeline(bad))
    intercept[IllegalArgumentException](CommandLine.parseCommandLine("a 'unterminated"))
    assertEquals(CommandLine.parseCommandLine("'/path with space/x' arg"), List("/path with space/x", "arg"))
    assertEquals(CommandLine.Stage(List("echo", "a b", "")).line, "echo \"a b\" \"\"")
    intercept[IllegalArgumentException](
      CommandLine.parsePipeline(List.fill(CommandLine.MaxPipelineStages + 1)("echo").mkString(" | "))
    )
    def glob(pattern: String)(path: String) = PathGlob.pattern(pattern).matcher(path).matches()
    val r = glob("src/**/*.scala")
    assert(r("src/X.scala") && r("src/a/b/X.scala") && !r("lib/X.scala") && !r("src/X.java"))
    assert(glob("**/test/*.py")("test/a.py") && glob("**/test/*.py")("x/y/test/a.py"))
    assert(glob("a/[!x]*.{md,txt}")("a/b.md") && !glob("a/[!x]*.{md,txt}")("a/x.md"))
    assert(!glob("a/*.md")("a/b/c.md"))
    assert(!glob("a[!x]b")("a/b") && !glob("a[+-0]b")("a/b")) // a class stays within one name
    assert(glob("{a,{b,c}}")("c"))
    for bad <- List("a/{b,c", "[abc", "[]]") do
      assert(intercept[IllegalArgumentException](PathGlob.pattern(bad)).getMessage.nn.contains("bad glob"), bad)
    assertEquals(glob("SRC/**/*.SCALA")("src/main/A.scala"), Platform.caseInsensitivePaths)

  test("Windows path validation rejects device aliases and ambiguous components"):
    for path <- List(
        "NUL.txt",
        "NUL .txt",
        "CON",
        "COM1.log",
        "COM¹.txt",
        "LPT³",
        "dir/name.",
        "dir/name ",
        "//?/C:/work/x",
        "//./pipe/x",
        "C:work/x",
        "C:/work/file.txt:stream",
      )
    do
      assert(PlatformPath.windowsValidationError(path).nonEmpty, path)
    for path <- List("C:/work/file.txt", ".env") do
      assertEquals(PlatformPath.windowsValidationError(path), None, path)
    // `parse` serves the host, the command line, `/save` and `/resume`.
    assertEquals(PlatformPath.parse("~/notes/x.json"), PlatformPath.userHome.resolve("notes").resolve("x.json"))
    if Platform.isWindows then
      assert(intercept[IllegalArgumentException](PlatformPath.parse("C:work")).getMessage.nn.contains("drive-relative"))
    assertEquals(
      ScalaSource.stringLiteral("C:\\Users\\alice\nnotes\".txt"),
      "\"C:\\\\Users\\\\alice\\nnotes\\\".txt\"",
    )

  test("a UNC path is refused before the file system (and so the network) is touched"):
    assume(Platform.isWindows, "UNC paths are Windows paths")
    for path <- List("//server.invalid/share/x", "\\\\server.invalid\\share\\x") do
      val error = intercept[IllegalArgumentException](exists(path))
      assert(error.getMessage.nn.contains("UNC path"), error.getMessage)

  test("readBytes/writeBytes round-trip a binary file byte for byte"):
    val bytes = Array[Byte](0, 1, 2, -1, -128, 127, 10, 13)
    Files.write(root.resolve("bin.dat"), bytes)
    assert(readBytes("bin.dat").sameElements(bytes))
    writeBytes("copy.dat", readBytes("bin.dat"))
    assert(Files.readAllBytes(root.resolve("copy.dat")).nn.sameElements(bytes))

  test("a literal Unix backslash in a filename survives an API round-trip"):
    assume(!Platform.isWindows)
    val name = "back\\slash.txt"
    write(name, "content")
    val returned = access(name).path
    assert(returned.endsWith(name), returned)
    assertEquals(read(returned), "content")

  test("parallel runs the tasks at once, keeps their order, and reports the first failure in task order"):
    val started = CountDownLatch(3)
    val results = parallel(List(1, 2, 3).map { i => () =>
      started.countDown()
      assert(started.await(5, TimeUnit.SECONDS), "the tasks did not run at once")
      i * 10
    })
    assertEquals(results, List(10, 20, 30))
    assertEquals(parallel(List.empty[() => Int]), Nil)
    val failure = intercept[IllegalStateException](parallel(List(1, 2, 3).map { i => () =>
      if i == 1 then i else throw IllegalStateException(s"task $i")
    }))
    assertEquals(failure.getMessage, "task 2")

  test("TODO updates from parallel tasks are not lost"):
    val items = (1 to Host.MaxParallel).map(i => s"item $i").toList
    for _ <- 1 to 50 do
      setTodos(items.map(Todo(_)))
      val ready = CountDownLatch(items.size)
      parallel(items.map { item => () =>
        ready.countDown()
        ready.await(5, TimeUnit.SECONDS)
        markTodo(item, TodoStatus.Done)
      })
      assertEquals(todos.map(_.status), items.map(_ => TodoStatus.Done))

  test("a nested parallel runs its tasks in order on the thread of the task that calls it"):
    // A pool per nesting level would multiply the threads of a recursion.
    val sameThread = parallel(List(1, 2).map { _ => () =>
      val outer = Thread.currentThread.getName
      parallel(List(1, 2, 3).map(_ => () => Thread.currentThread.getName)).map(_ == outer)
    })
    assertEquals(sameThread, List(List(true, true, true), List(true, true, true)))
    val ran = java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val failure = intercept[IllegalStateException](parallel(List(() =>
      parallel(List(1, 2, 3).map { i => () =>
        ran.add(i)
        if i == 1 then i else throw IllegalStateException(s"inner $i")
      })
    )))
    assertEquals(failure.getMessage, "inner 2")
    assertEquals(ran.toArray.toList, List(1, 2, 3)) // every task ran, in order

  test("parallel lets a fatal throwable win and passes an interruption on to its tasks"):
    // munit's intercept only catches non-fatal errors, and the stop signal is fatal by design.
    val fatal =
      try
        parallel(List(() => throw IllegalStateException("ordinary"), () => throw ThreadDeath()))
        None
      catch case stop: ThreadDeath => Some(stop)
    assert(fatal.isDefined, "the fatal throwable did not win over the ordinary failure")
    val sleeping = CountDownLatch(1)
    val taskInterrupted = CountDownLatch(1)
    @volatile var outcome: Option[Exception] = None
    val caller = Thread(() =>
      try
        parallel(List(() =>
          sleeping.countDown()
          try Thread.sleep(10000)
          catch case _: InterruptedException => taskInterrupted.countDown()
          0
        ))
        ()
      catch case e: Exception => outcome = Some(e)
    )
    caller.setDaemon(true)
    caller.start()
    assert(sleeping.await(5, TimeUnit.SECONDS))
    caller.interrupt()
    caller.join(5000)
    assert(!caller.isAlive, "the caller did not return after the interrupt")
    assert(outcome.exists(_.isInstanceOf[InterruptedException]), outcome.toString)
    assert(taskInterrupted.await(5, TimeUnit.SECONDS), "the task was not interrupted")

  test("writeBytes is refused on a classified path, like write"):
    intercept[SecurityException](writeBytes("secrets/x.dat", Array[Byte](1, 2)))

  test("forEachLine streams lines with 1-based numbers"):
    write("lines.txt", "alpha\nbeta\ngamma")
    val seen = ListBuffer[(String, Int)]()
    access("lines.txt").forEachLine((line, n) => seen += ((line, n)))
    assertEquals(seen.toList, List(("alpha", 1), ("beta", 2), ("gamma", 3)))

  test("forEachLine and grep tolerate non-UTF-8 bytes like read() does (no abort on binary files)"):
    val bytes = "ok\n".getBytes("UTF-8") ++ Array[Byte](0xff.toByte, 0xfe.toByte) ++ " bad\nend\n".getBytes("UTF-8")
    Files.write(root.resolve("latin.txt"), bytes)
    val seen = ListBuffer[String]()
    access("latin.txt").forEachLine((line, _) => seen += line)
    assertEquals(seen.size, 3)
    assertEquals(seen.head, "ok")
    assertEquals(seen.last, "end")
    assertEquals(seen(1), read("latin.txt").linesIterator.toList(1)) // same replacement as read()
    assertEquals(grep("latin.txt", "end").map(_.lineNumber), List(3))
    assert(grepRecursive(".", "ok").exists(_.file.endsWith("latin.txt")))

  test("outside cwd is denied with a request hint"):
    val e = intercept[SecurityException](read("/etc/hosts"))
    assert(e.getMessage.nn.contains("requestFiles"), e.getMessage)

  test("classified: plain read denied, classified read ok, listing rules"):
    val e = intercept[SecurityException](read("secrets/key.txt"))
    assert(e.getMessage.nn.contains("readClassified"))
    assertEquals(ClassifiedImpl.get(readClassified("secrets/key.txt")), "s3cret")
    assert(access("secrets/key.txt").isClassified)
    intercept[SecurityException](access("secrets/key.txt").size)
    // the classified dir itself is visible in its parent, but not enterable
    val top = ls(".").map(p => Path.of(p).getFileName.toString)
    assert(top.contains("secrets"))
    assert(!top.contains("private"), top.toString) // no access -> invisible
    intercept[SecurityException](ls("secrets"))
    val inside = ClassifiedImpl.get(access("secrets").childrenClassified).map(p => Path.of(p).getFileName.toString)
    assertEquals(inside.sorted, List("key.txt", "sub"))
    val walked = walk(".").map(env.rel)
    assert(walked.contains("secrets"))
    assert(!walked.exists(_.startsWith("secrets/")), walked.toString)
    assert(!walked.exists(_.startsWith("private")))
    val walkedC = ClassifiedImpl.get(access(".").walkClassified()).map(env.rel)
    assert(walkedC.contains("secrets/sub/deep.txt"))

  test("a denial names the operation the agent called, and reads as a phrase in front of the path"):
    val calls: List[(String, () => Any)] = List(
      "read" -> (() => read("secrets/key.txt")),
      "cat" -> (() => cat("secrets/key.txt")),
      "cat" -> (() => cat("secrets/key.txt", 1, 2)),
      "readRange" -> (() => readRange("secrets/key.txt", 1, 2)),
      "grep" -> (() => grep("secrets/key.txt", "s")),
      "forEachLine" -> (() => access("secrets/key.txt").forEachLine((_, _) => ())),
    )
    for (operation, call) <- calls do
      val message = intercept[SecurityException](call()).getMessage.nn
      assert(message.contains(s"'$operation' would reveal its content"), message)
    val outside = intercept[SecurityException](read("/etc/hosts")).getMessage.nn
    assert(outside.startsWith("Access denied: read '"), outside)
    assert(outside.contains("' is not permitted (current permission: none)"), outside)

  test("classified writes"):
    intercept[SecurityException](write("secrets/new.txt", "x"))
    writeClassified("secrets/new.txt", classify("x"))
    assertEquals(Files.readString(root.resolve("secrets/new.txt")), "x")
    // declassification by writing into an unclassified path is refused
    intercept[SecurityException](writeClassified("src/leak.txt", classify("x")))
    assert(!Files.exists(root.resolve("src/leak.txt")))

  test("request scope grants access outside cwd once"):
    val other = TestEnv.outsideDir()
    env.decisions = List(Decision.AllowOnce)
    val got = requestFiles(other.toString, atc.lib.Access.Read, "test") {
      intercept[SecurityException](write(s"$other/w.txt", "x"))
      read(s"$other/o.txt")
    }
    assertEquals(got, "outside")
    intercept[SecurityException](read(s"$other/o.txt"))
    env.decisions = Nil
    intercept[SecurityException](requestFiles(other.toString, atc.lib.Access.Read, "again") { 1 })

  test("exec policy and output"):
    val r = exec(echoCommand, List("hi there"))
    assertEquals(r.exitCode, 0)
    assertEquals(r.stdout.trim, "hi there")
    val pwd = ProcessFixture.command("pwd")
    val pattern = ProcessFixture.pattern("pwd")
    val e = intercept[SecurityException](exec(pwd))
    assert(e.getMessage.nn.contains("requestExec"))
    env.decisions = List(Decision.AllowSession)
    assertEquals(requestExec(Set(pattern), "inspect cwd") { exec(pwd).exitCode }, 0)
    assertEquals(exec(pwd, Nil, root.toString).exitCode, 0) // session grant persists

  test("commands do not inherit the variables that hold provider keys"):
    val plain = TestEnv(commands = List(ProcessFixture.pattern("env")))
    val keyed = Host(plain.policy, plain.root, plain.output, plain.llm, plain.ui, keyVariables = () => Set("PATH"))
    val printPath = ProcessFixture.command("env", "PATH")
    import plain.given
    assertEquals(plain.host.exec(printPath).stdout.trim, "<set>")
    assertEquals(keyed.exec(printPath).stdout.trim, "<unset>")

  test("a denied executable path gets a copyable requestExec hint"):
    val executable = if Platform.isWindows then "C:\\Program Files\\Example\\tool.exe" else "/opt/Example Tools/tool"
    val error = intercept[SecurityException](exec(s"'$executable' --status"))
    val literal = ScalaSource.stringLiteral(CommandLine.Stage(List(executable, "--status")).line)
    assert(error.getMessage.nn.contains(s"requestExec(Set($literal)"), error.getMessage)

  test("a command that runs long is shown live after Processes.LiveAfterMs, a quick one is not"):
    assume(!Platform.isWindows) // intentional integration with the real POSIX shell
    val begun = ListBuffer[Long]()
    val seen = StringBuilder()
    val live = new Processes.LiveOutput:
      def begin(): Unit = begun += System.nanoTime()
      def output(text: String): Unit = seen.synchronized(seen.append(text))
    def sh(script: String) =
      ProcessFixture.run(ProcessBuilder("sh", "-c", script), "sh", 10_000L, Some(live))
    val quick = sh("echo quick")
    assertEquals(quick.stdout, "quick\n")
    assertEquals(begun.size, 0) // done before the live threshold: nothing shown
    assertEquals(seen.toString, "")
    val start = System.nanoTime()
    val slow = sh("echo early; echo err >&2; sleep 1.6; echo late")
    assertEquals(slow.stdout, "early\nlate\n")
    assertEquals(slow.stderr, "err\n")
    assertEquals(begun.size, 1, "went live once")
    val liveAfterMs = (begun.head - start) / 1_000_000L
    assert(liveAfterMs >= Processes.LiveAfterMs - 50 && liveAfterMs < 1500, s"went live after $liveAfterMs ms")
    // The early output (both streams) is delivered when it goes live, the rest as it comes.
    val text = seen.toString
    assert(text.contains("early\n") && text.contains("err\n") && text.endsWith("late\n"), text)
    // Through the host, the port hears the command line and its output.
    val shell = TestEnv(commands = List("sh *"))
    import shell.given
    shell.host.exec("sh", List("-c", "echo a; sleep 1.3; echo b"))
    assertEquals(shell.liveCommands.toList, List("sh -c \"echo a; sleep 1.3; echo b\""))
    assertEquals(shell.liveCommandOut.toString, "a\nb\n")
    shell.host.exec("sh", List("-c", "echo nope"))
    assertEquals(shell.liveCommands.size, 1)

  test("deny lists refuse commands and hosts, and cannot be granted"):
    val denied = ProcessFixture.command("echo", "secret") + "*"
    val deny = TestEnv(
      commands = List(echoPattern), // allowed by the allow list ...
      hosts = List("*"),
      denyCommands = List(denied), // ... but these are refused outright
      denyHosts = List("*.internal"),
    )
    deny.decisions = List.fill(2)(Decision.AllowSession) // ... and the user would say yes to anything
    import deny.given
    assertEquals(deny.host.exec(echoCommand, List("ok")).stdout.trim, "ok")
    val e = intercept[SecurityException](deny.host.exec(echoCommand, List("secret", "key")))
    assert(e.getMessage.nn.contains(s"denyCommands pattern '$denied'"), e.getMessage)
    assert(!e.getMessage.nn.contains("requestExec"), e.getMessage) // asking cannot help
    val e2 = intercept[SecurityException](deny.host.requestExec(Set(denied), "try") { 1 })
    assert(e2.getMessage.nn.contains("may not be granted"), e2.getMessage)
    val e3 = intercept[SecurityException](deny.host.httpGet("http://db.internal/x"))
    assert(e3.getMessage.nn.contains("denyHosts pattern '*.internal'"), e3.getMessage)
    val e4 = intercept[SecurityException](deny.host.requestNetwork(Set("db.internal"), "try") { 1 })
    assert(e4.getMessage.nn.contains("may not be granted"), e4.getMessage)
    assertEquals(deny.policy.openScopeCount, 0)

  test("an IPv6-mapped spelling of a denied IPv4 host is refused, not just the IPv4 form"):
    // Every host is allowed except this one, however it is spelled.
    val deny = TestEnv(hosts = List("*"), denyHosts = List("169.254.169.254"))
    import deny.given
    val e = intercept[SecurityException](deny.host.httpGet("http://[::ffff:169.254.169.254]/latest/meta-data/"))
    assert(e.getMessage.nn.contains("denyHosts pattern"), e.getMessage)

  test("network host policy"):
    val e = intercept[SecurityException](httpGet("http://nope.invalid/x"))
    assert(e.getMessage.nn.contains("requestNetwork"))

  test("print splits agent and user views"):
    println(classify("real"))
    println("same")
    assertEquals(env.agentOut.toString, "Classified(***)\nsame\n")
    assertEquals(env.userOut.toString, "<real\n>same\n")

  test("entries listed through children/walk are judged by their target, like paths given by name"):
    // A link in a readable directory to a classified file: `read` by name is
    // refused, and an entry obtained from a listing must be refused the same way,
    // so the entry carries the target's path and is judged there.
    val linked = TestEnv(TestEnv.withSecrets)
    import linked.given
    linked.file("secrets/key.txt", "THE-SECRET")
    linked.dir("pub")
    assume(
      TestEnv.trySymbolicLink(linked.root.resolve("pub/link.txt"), linked.root.resolve("secrets/key.txt")),
      "symbolic links are unavailable for this account",
    )
    val target = PlatformPath.portable(linked.root.resolve("secrets/key.txt"))
    intercept[SecurityException](linked.host.read("pub/link.txt"))
    val listed = linked.host.access("pub").children
    assertEquals(listed.map(_.path), List(target), "a link is listed as its target")
    assert(listed.head.isClassified)
    intercept[SecurityException](listed.head.read())
    intercept[SecurityException](linked.host.access("pub").walk().head.read())
    // `ls` shows the same canonical path (relative to the root, as listings are), and
    // `readClassified` stays the way in.
    assertEquals(linked.host.ls("pub"), List("secrets/key.txt"))
    linked.host.readClassified(listed.head.path)

  test("symlink escaping cwd is judged by its target"):
    val outside = TestEnv.outsideDir()
    assume(TestEnv.trySymbolicLink(root.resolve("link"), outside), "symbolic links are unavailable for this account")
    intercept[SecurityException](read("link/o.txt"))
    val top = ls(".").map(p => Path.of(p).getFileName.toString)
    assert(!top.contains("link"), top.toString)

  test("a Windows directory junction escaping cwd is judged by its target"):
    assume(Platform.isWindows, "Windows junction integration test")
    val outside = TestEnv.outsideDir()
    val junction = root.resolve("junction-out").nn
    val process = ProcessBuilder("cmd.exe", "/d", "/c", "mklink", "/J", junction.toString, outside.toString)
      .redirectErrorStream(true).start().nn
    val output = String(process.getInputStream.nn.readAllBytes(), Charset.defaultCharset())
    val exit = process.waitFor()
    assertEquals(exit, 0, s"could not create a junction: $output")
    assert(Files.isDirectory(junction, LinkOption.NOFOLLOW_LINKS), s"junction was not created: $output")
    intercept[SecurityException](read("junction-out/o.txt"))
    assert(!ls(".").exists(_.contains("junction-out")), ls(".").toString)

  test("a dangling symlink is judged by its (non-existent) target"):
    // Writing through a dangling link creates its target. The policy must evaluate
    // that target even before it exists, or a link in an allowed tree could create
    // a file anywhere.
    val target = TestEnv.outsideDir().resolve("created.txt") // does not exist
    assume(
      TestEnv.trySymbolicLink(root.resolve("dangling.txt"), target),
      "symbolic links are unavailable for this account",
    )
    intercept[SecurityException](write("dangling.txt", "PWNED"))
    intercept[SecurityException](append("dangling.txt", "PWNED"))
    assert(!Files.exists(target))
    // A dangling symlink into the writable tree remains writable.
    val inner = root.resolve("inner-created.txt")
    assert(TestEnv.trySymbolicLink(root.resolve("dangling-ok.txt"), inner))
    write("dangling-ok.txt", "fine")
    assertEquals(Files.readString(inner), "fine")

  test("a readable symlinked directory is listed as its target but never entered"):
    Files.createDirectories(root.resolve("real/sub"))
    Files.writeString(root.resolve("real/sub/f.txt"), "f")
    assume(
      TestEnv.trySymbolicLink(root.resolve("dirlink"), root.resolve("real")),
      "symbolic links are unavailable for this account",
    )
    // List and evaluate the link as its target.
    val top = ls(".")
    assert(top.contains("real"), top.toString)
    assertEquals(top.count(_ == "real"), 1, top.toString) // the link resolves to it: listed once
    assert(!top.exists(_.contains("dirlink")), top.toString)
    // Walk the real directory once without traversing the link.
    val walked = walk(".").map(env.rel)
    assert(walked.contains("real/sub/f.txt"), walked.toString)
    assert(!walked.exists(_.contains("dirlink")), walked.toString)
    assertEquals(walked.count(_ == "real/sub/f.txt"), 1, walked.toString)
    // Evaluate access through the link at its target.
    assertEquals(read("dirlink/sub/f.txt"), "f")
    assertEquals(access("dirlink/sub/f.txt").path, PlatformPath.portable(root.resolve("real/sub/f.txt")))

  test("move of a file onto itself is a no-op"):
    write("self.txt", "data")
    move("self.txt", "./self.txt")
    assertEquals(read("self.txt"), "data")
