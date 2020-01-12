
package swam
package slumps
package test

import better.files.File
import cats.effect._
import swam.binary.ModuleParser
import swam.slumps.Slumps
import swam.slumps.internals.Souper
import swam.test.util.testfiles
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

  def runSouper(candidate: File): Unit ={
    println(souper.inferLHS(candidate.contentAsString()))
  }


  val tests = Tests {
    "Souper IR" - runSouper(better.files.File("slumps/test/resources/souper/souperIr1.txt"))
  }


}
