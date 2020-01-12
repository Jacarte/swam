// Utility class for mapping between Wasm and Souper IR
// https://github.com/google/souper/blob/master/docs/InstRef.md#instructions


package swam
package slumps
package internals


import java.io._
import config._
import pureconfig._
import pureconfig.generic.auto._

import scala.sys.process._

class Souper private (val conf: SlumpsConfiguration)(implicit parser: SouperParser) {

  private def getInput(in:String): InputStream = new ByteArrayInputStream(in.getBytes("UTF-8"))


  def inferRHS(LHS: String):String = (s"${conf.souperBinPath}/souper-check ${conf.souperArgs.mkString(" ")} -z3-path=${conf.zSolver} -" #< getInput(LHS)).lazyLines_!.mkString("\n")

}

object Souper {

  def apply(): Souper = {

    implicit val parser: SouperParser = new SouperParser

    val slumps = ConfigSource.default.at("swam.slumps").load[SlumpsConfiguration] match {
      case Right(conf) => conf
      case Left(failures) => throw new Exception(s"Unable to load the configuration ${failures.toList.mkString("\n")}")
    }

    new Souper(slumps)
  }

}