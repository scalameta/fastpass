package scala.meta.internal.fastpass

import java.nio.file.Path
import scala.meta.internal.fastpass.pantsbuild.MessageOnlyException
import scala.concurrent.ExecutionContext
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import scala.sys.process._

object SystemProcess {
  def run(
      shortName: String,
      args: List[String],
      reproduceArgs: List[String],
      cwd: Path,
      token: CancelChecker
  )(implicit ec: ExecutionContext): Unit = {
    val exportTimer = new Timer(Time.system)
    scribe.info(args.mkString("process: ", " ", ""))
    val exit = Process(args, cwd = Some(cwd.toFile())).!
    if (exit != 0) {
      val message = s"$shortName command failed with exit code $exit, " +
        s"to reproduce run the command below:\n\t${reproduceArgs.mkString(" ")}"
      throw MessageOnlyException(message)
    } else {
      scribe.info(s"time: ran '$shortName' in $exportTimer")
    }
  }
}
