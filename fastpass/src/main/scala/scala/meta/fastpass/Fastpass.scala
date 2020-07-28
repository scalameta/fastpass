package scala.meta.fastpass

import scala.meta.internal.fastpass.SystemProcess
import scala.meta.internal.fastpass.pantsbuild.BloopPants

class Fastpass
object Fastpass {
  def main(args: Array[String]): Unit = {
    BloopPants.main(args)
  }

  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      SystemProcess.RunningProcesses.destroyAll()
    }
  })
}
