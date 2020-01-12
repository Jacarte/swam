
package swam
package slumps
package test

import cats.effect._
import swam.binary.ModuleParser
import swam.slumps.Slumps
import swam.slumps.internals.Souper
import swam.{binary, slumps, validation}
import swam.validation.Validator
import utest._

import scala.concurrent.ExecutionContext



object SouperTests extends TestSuite {


  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)


  val slumps = Slumps[IO]
  var souper = Souper.apply()

  def run(wast: String) = {

      slumps
  }

  def runSouper(): Unit ={
    souper.inferLHS("Test")
  }

  val tests = Tests{
    "Calling Souper" - runSouper()
  }

}
