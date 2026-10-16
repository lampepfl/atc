package atc

private[atc] object Threads:
  /** A daemon thread named `name` that runs `work`; the caller starts it. */
  def daemon(name: String)(work: Runnable): Thread =
    val thread = Thread(work, name)
    thread.setDaemon(true)
    thread
