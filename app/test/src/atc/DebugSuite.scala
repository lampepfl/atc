package atc

class DebugSuite extends munit.FunSuite:
  test("debug messages are evaluated once when enabled and never when disabled"):
    var evaluations = 0
    Debug.log {
      evaluations += 1
      "checking lazy debug output"
    }
    assertEquals(evaluations, if Debug.enabled then 1 else 0)

  test("error descriptions retain the cause hidden by a generic request failure"):
    val error = RuntimeException("Request failed", java.io.IOException("executor rejected"))
    assertEquals(Debug.describe(error), "RuntimeException: Request failed\nCaused by: IOException: executor rejected")

  test("error descriptions handle absent messages and cyclic cause chains"):
    val first = RuntimeException()
    val second = RuntimeException("nested", first)
    first.initCause(second)
    assertEquals(Debug.describe(first), "RuntimeException\nCaused by: RuntimeException: nested")

  test("a file system exception that names only a path says what went wrong with it"):
    assertEquals(
      Debug.message(java.nio.file.NoSuchFileException("/x/missing.json")),
      "/x/missing.json: no such file or directory"
    )
    assertEquals(Debug.message(java.nio.file.AccessDeniedException("/x/secret")), "/x/secret: access denied")
    assertEquals(
      Debug.message(java.nio.file.FileSystemException("/x/loop", null, "Too many levels")),
      "/x/loop: Too many levels"
    )
    assertEquals(Debug.message(RuntimeException()), "RuntimeException")

  test("logged text loses its terminal controls"):
    assertEquals(
      Debug.line("provider said \u001b]0;evil\u0007\u001b[2Jhi\nnext"),
      "[atc] provider said ]0;evil[2Jhi\nnext"
    )
