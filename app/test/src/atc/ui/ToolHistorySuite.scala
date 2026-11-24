package atc.ui

import atc.host.FileChange
import atc.sandbox.ExecutionResult

class ToolHistorySuite extends munit.FunSuite:
  test("retained output and file previews remain available without rerunning tools"):
    val history = ToolHistory()
    val entry = history.add(
      "run_scala",
      "write(...)\n42",
      ExecutionResult(true, "42"),
      10,
      "command output",
      List(FileChange("file.txt", "updated", "- old\n+ new"))
    )
    val retained = history.get(entry.id).get
    assertEquals(retained.output, "42")
    assertEquals(retained.live, "command output")
    assertEquals(retained.changes.map(_.preview), List("- old\n+ new"))
    assertEquals(
      history.entries.map(e => (e.id, e.title, e.success, e.millis)),
      Vector((entry.id, "run_scala", true, 10L))
    )

  test("old output is evicted within the configured retention budget"):
    val history = ToolHistory(maxChars = 500, maxEntries = 3)
    val first = history.add("run_scala", "first", ExecutionResult(true, "x" * 1000), 1, "", Nil)
    for i <- 1 to 20 do history.add("run_scala", s"tool $i", ExecutionResult(true, "x" * 1000), 1, "", Nil)
    assertEquals(history.get(first.id), None)
    assert(history.entries.size <= 3)
    assert(history.latest.get.output.contains("retained output limit reached"))
    history.clear()
    assertEquals(history.latest, None)

  test("the result and the command output share one output limit"):
    val history = ToolHistory(maxChars = 200)
    val entry = history.add("run_scala", "x", ExecutionResult(true, "r" * 60), 1, "c" * 60, Nil)
    assertEquals(entry.output, "r" * 60)
    assertEquals(entry.live, "c" * 40 + "\n[retained output limit reached]")
