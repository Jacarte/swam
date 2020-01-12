// Utility class for mapping between Wasm and Souper IR
// https://github.com/google/souper/blob/master/docs/InstRef.md#instructions


package swam
package slumps
package internals


import cats.effect.Effect
import config._
import pureconfig._
import pureconfig.generic.auto._

class Souper private (val conf: SlumpsConfiguration) {

  def inferLHS(LHS: String):String = {
    println(conf.souperArgs)
    ""
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