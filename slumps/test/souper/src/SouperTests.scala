
package swam
package slumps
package test

import better.files.File
import cats.effect._
import swam.binary.ModuleParser
import swam.slumps.Slumps
import swam.slumps.internals.{Souper, SouperParser}
import swam.test.util.testfiles
import swam.{binary, slumps, validation}
import swam.validation.Validator
import utest._

import fastparse._

import scala.concurrent.ExecutionContext



object SouperTests extends TestSuite {


  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)


  val slumps = Slumps[IO]
  val parser = new SouperParser
  var souper = Souper.apply()

  def run(wast: String) = {

      slumps
  }

  def runSouper(candidate: File): Unit ={
    println(souper.inferRHS(candidate.contentAsString()))
  }


  def runParser(candidate: File, mergeResult: File): Unit ={

    val graph = parse(candidate.contentAsString, parser.dag(_)).get.value

    println(graph.entry)

  }



  val tests = Tests {
    "Souper IR" - runSouper(better.files.File("slumps/test/resources/souper/souperIr1.txt"))
    "Parsing and merging" - runParser(better.files.File("slumps/test/resources/souper/souperIr3.txt"),
      better.files.File("slumps/test/resources/souper/souperIr2.txt"))

  }


}
