package atc

/** The API reference the model reads: the compact form of the bundled `Interface.scala`
  * and the documentation `help` prints. */
class ApiReferenceSuite extends munit.FunSuite:
  private val full = ApiReference.full
  private val compact = ApiReference.compact

  test("the reference is bundled"):
    assert(full.contains("trait Interface:"), full.take(200))

  test("the compact reference keeps every public method's signature"):
    val signatures = full.linesIterator.map(_.trim)
      .filter(l => l.startsWith("def ") || l.startsWith("update def "))
      .map(l => l.indexOf(" = ") match { case -1 => l; case body => l.take(body) })
      .toList
    assert(signatures.size > 100, signatures.size)
    val kept = compact.linesIterator.map(_.trim).toSet
    assertEquals(signatures.filterNot(kept), Nil)

  test("the compact reference drops examples, bodies, annotations and imports, and is much smaller"):
    for noise <- List("@assumeSafe", "import ", "this match", "private[atc] ()", "private def") do
      assert(!compact.contains(noise), noise)
    assertEquals(compact.split("\\{\\{\\{").length - 1, 3, "only the docs kept whole keep their examples")
    assert(compact.length * 10 < full.length * 7, s"${compact.length} of ${full.length}")
    assert(compact.contains("case class ProcessResult(exitCode: Int, stdout: String, stderr: String)\n"))
    assert(compact.contains("def zip[B](that: Classified[B]): Classified[(T, B)]\n"))

  test("a doc comment keeps its first sentence, with a mark when more was cut"):
    assert(compact.contains("/** Read a whole file. */"))
    assert(compact.contains("/** Start a `Process` using `exec`'s grammar and checks. … */"))
    assert(compact.contains("/** Minimal JSON. … */"), "a cut before a code example is marked")

  test("the docs marked to stay whole do, and neither form shows the marker"):
    for kept <- List(
        "A leading `NAME=value` is refused: use `withEnv`.", // exec
        "The only supported destinations are `println`", // Classified
        "they MUST be independent", // parallel
      )
    do assert(compact.contains(kept), kept)
    assert(ApiReference.bundled.contains(ApiReference.KeepWhole), "the source marks them")
    assert(!full.contains(ApiReference.KeepWhole) && !compact.contains(ApiReference.KeepWhole))

  test("a sentence ends outside backticks and after a word that is not an abbreviation"):
    assertEquals(ApiReference.firstSentence("Use `a.b`. Then more."), "Use `a.b`.")
    assertEquals(ApiReference.firstSentence("A list, e.g. this one. More."), "A list, e.g. this one.")
    assertEquals(ApiReference.firstSentence("No full stop"), "No full stop")

  test("help gives a method's whole doc, its overloads together"):
    val sed = ApiReference.help(full, "sed")
    assert(sed.contains("It throws `IllegalArgumentException` when\n *  nothing matches"), sed)
    assert(sed.contains("quoteReplacement(\"foo(a, b, c)\")"), "the example stays")
    val exec = ApiReference.help(full, "exec")
    assertEquals(exec.linesIterator.count(_.startsWith("def exec(")), 4)
    assert(!exec.contains("\n\n"), "one block")

  test("help labels a name that several types declare, and Type.member picks one"):
    val read = ApiReference.help(full, "read")
    for label <- List("// read", "// FileEntry.read", "// Process.read") do assert(read.contains(label), read)
    assertEquals(
      ApiReference.help(full, "Process.read"),
      "/** Stdout produced since the last read, consumed; \"\" when there is none. Never blocks. */\ndef read(): String",
    )

  test("help gives a type with its members' docs, and suggests names for an unknown one"):
    val process = ApiReference.help(full, "Process")
    assert(process.contains("abstract class Process extends caps.ExclusiveCapability:"), process)
    assert(process.contains("/** Wait up to `timeoutMs` for the process to exit"), process)
    assertEquals(ApiReference.help(full, "grepp"), "No API name `grepp`; similar names: grep.")
    assertEquals(ApiReference.help(full, "nothing"), "No API name `nothing`.")
