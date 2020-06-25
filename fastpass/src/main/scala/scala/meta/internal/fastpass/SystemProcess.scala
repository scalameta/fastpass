package scala.meta.internal.fastpass

import java.io.PrintStream
import java.nio.file.Path

import scala.concurrent.ExecutionContext
import scala.sys.process._

import scala.meta.internal.fastpass.pantsbuild.MessageOnlyException

import org.eclipse.lsp4j.jsonrpc.CancelChecker

object SystemProcess {
  def run(
      shortName: String,
      args: List[String],
      reproduceArgs: List[String],
      cwd: Path,
      token: CancelChecker,
      out: PrintStream,
      err: PrintStream
  )(implicit ec: ExecutionContext): Unit = {
    val processLogger = ProcessLogger(
      line => out.println(line),
      line => err.println(line)
    )
    val exportTimer = new Timer(Time.system)
    scribe.info(args.mkString("process: ", " ", ""))
    val exit = Process(args, cwd = Some(cwd.toFile())).!(processLogger)
    if (exit != 0) {
      val message = s"$shortName command failed with exit code $exit, " +
        s"to reproduce run the command below:\n\t${reproduceArgs.mkString(" ")}"
      throw MessageOnlyException(message)
    } else {
      scribe.info(s"time: ran '$shortName' in $exportTimer")
    }
  }
}
