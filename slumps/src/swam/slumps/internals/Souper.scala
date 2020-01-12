// Utility class for mapping between Wasm and Souper IR
// https://github.com/google/souper/blob/master/docs/InstRef.md#instructions


package swam
package slumps
package internals


import java.io.{ByteArrayInputStream, File, InputStream, StringBufferInputStream, StringReader}
import java.net.URL

import cats.effect.Effect
import config._
import pureconfig._
import pureconfig.generic.auto._

import scala.sys._
import scala.sys.process._

class Souper private (val conf: SlumpsConfiguration) {

  private def getInput(in:String): InputStream = new ByteArrayInputStream(in.getBytes("UTF-8"))

  def inferLHS(LHS: String):String = {

    println(LHS)
    (s"${conf.souperBinPath}/souper-check ${conf.souperArgs.mkString(" ")} -z3-path=${conf.zSolver} -" #< getInput(LHS)).lazyLines_!.mkString("\n")

  }

}

object Souper {

  def apply(): Souper = {
    val slumps = ConfigSource.default.at("swam.slumps").load[SlumpsConfiguration] match {
      case Right(conf) => conf
      case Left(failures) => throw new Exception(s"Unable to load the configuration ${failures.toList.mkString("\n")}")
    }

    new Souper(slumps)
  }

}