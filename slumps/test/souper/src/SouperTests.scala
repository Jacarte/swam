
package swam
package slumps
package test

import java.nio.ByteBuffer

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
import swam.runtime.{Engine, Instance}

import scala.concurrent.ExecutionContext
import runtime._
import formats.DefaultFormatters._
import cats.effect._
import java.nio.file.Paths
import java.util.concurrent.Executors

import cats.effect.{Blocker, ContextShift, IO}
import swam.runtime.imports.{AsInstance, AsInterface, Elem, Imports, TCMap}


object SouperTests extends TestSuite {


  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  val slumps = Slumps[IO]
  val parser = new SouperParser
  var souper = Souper.apply()

  def run(wast: String) = {

      slumps
  }


  val blockingPool = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  val blocker: Blocker = Blocker.liftExecutionContext(blockingPool)

  val engine = Engine[IO]


  def printf(l: Any): IO[Unit] =
    IO(println(l))


  def printi32(i: Int): IO[Unit] =
    IO(println(i))

  def printi32f32(i: Int, f: Float): IO[Unit] =
    IO(println(s"$i $f"))

  def printi64(l: Long): IO[Unit] =
    IO(println(l))

  def printf32(f: Float): IO[Unit] =
    IO(println(f))

  def printf64(d: Double): IO[Unit] =
    IO(println(d))

  def printf64f64(d1: Double, d2: Double): IO[Unit] =
    IO(println(s"$d1 $d2"))

  def print(i: Int, j: Int): IO[Int] = {
    IO({println("print");  0})
  }

  def buffer = {
    val buffer = ByteBuffer.allocate(2 * pageSize)
    buffer.limit(pageSize)
    buffer
  }

  def stdlib =
    Imports[IO](
      TCMap[String, AsIsIO]("env" -> TCMap[String, AsIIO](
        "memory" -> buffer,
        "printf" -> print _
      )))

  def instantiate(p: String): Instance[IO] =
    (for {
      engine <- engine
      m <- engine.instantiateBytes(fs2.io.file.readAll[IO](Paths.get(p), blocker, 4096),stdlib)
    } yield m).unsafeRunSync()

  def time[T](t: => T): T = {
    val start = System.currentTimeMillis
    val res = t
    val end = System.currentTimeMillis
    println(s"Time: ${end - start}ms")
    res
  }

  def runAndTrace(f: File): Unit = {

    val i = instantiate(f.path.toString)

    val main = i.exports.typed.function2[Int, Int, Int]("main").unsafeRunSync()

  }


  val tests = Tests {
    "babbage problem" - runAndTrace(better.files.File("slumps/test/resources/slumps/babbage_problem[0].wasm"))
  }


}
