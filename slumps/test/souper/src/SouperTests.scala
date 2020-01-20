package swam
package slumps
package test

import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.file.Paths
import java.util.concurrent.Executors

import better.files.File
import cats.effect.{Blocker, IO}
import swam.runtime.formats.DefaultFormatters._
import swam.runtime.imports.{AsInstance, AsInterface, Imports, TCMap}
import swam.runtime.trace.{EventType, Tracer}
import swam.runtime.{Engine, Instance, _}
import utest._

import scala.concurrent.ExecutionContext

class STRACTracer(val file: PrintWriter) extends Tracer {
  override def traceEvent(tpe: EventType, args: List[String]): Unit = {
    file.write(s"${tpe.entryName},${args.mkString(",")}\n")
  }
}

object SouperTests extends TestSuite {

  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  val blockingPool = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  val blocker: Blocker = Blocker.liftExecutionContext(blockingPool)

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
    IO({ println("print"); 0 })
  }

  def buffer = {
    val buffer = ByteBuffer.allocate(2 * pageSize)
    buffer.limit(pageSize)
    buffer
  }

  def stdlib =
    Imports[IO](
      TCMap[String, AsIsIO](
        "env" -> TCMap[String, AsIIO](
          "memory" -> buffer,
          "printf" -> print _
        )))

  def instantiate(p: String, tr: Tracer): Instance[IO] =
    (for {
      engine <- Engine[IO](Option(tr))
      m <- engine.instantiateBytes(fs2.io.file.readAll[IO](Paths.get(p), blocker, 4096), stdlib)
    } yield m).unsafeRunSync()

  def time[T](t: => T): T = {
    val start = System.currentTimeMillis
    val res = t
    val end = System.currentTimeMillis
    println(s"Time: ${end - start}ms")
    res
  }

  def runAndTrace(name: String, f: File): Unit = {

    val file = new PrintWriter(new java.io.File(s"${name}.strac.log"))

    val tracer = new STRACTracer(file)

    val i = instantiate(f.path.toString, tracer)

    i.interpreter.interpret(2, Vector(), i)

    println(i.module.functions.toList.map(t => t.code.length).sum)

    file.flush()
    file.close()

    //println(i.exports.function("__original_main").unsafeRunSync())

  }

  val tests = Tests {
    "babbage problem [0]" - runAndTrace("bp[0]",
                                        better.files.File("slumps/test/resources/slumps/babbage_problem[0].wasm"))
    "babbage problem" - runAndTrace("bp", better.files.File("slumps/test/resources/slumps/babbage_problem.wasm"))
    "babbage problem [2]" - runAndTrace("bp[2]",
                                        better.files.File("slumps/test/resources/slumps/babbage_problem[2].wasm"))
    "babbage problem [1]" - runAndTrace("bp[1]",
                                        better.files.File("slumps/test/resources/slumps/babbage_problem[1].wasm"))
    // "Bitwise OI [10]" - runAndTrace(better.files.File("slumps/test/resources/slumps/bitwise_IO[10].wasm"))
    // "Bitwie IO[5 7 9]" - runAndTrace(better.files.File("slumps/test/resources/slumps/bitwise_IO[5_7_9].wasm"))
  }

}
