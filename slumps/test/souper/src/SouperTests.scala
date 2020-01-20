package swam
package slumps
package test

import java.io.PrintWriter
import java.nio.ByteBuffer
import java.nio.file.Paths
import java.util.concurrent.Executors

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

  val tests = Tests {
    def runAndTrace()(implicit path: utest.framework.TestPath): Unit = {
      val fname = path.value.last.toString()
      val logFile = new PrintWriter(new java.io.File(s"${fname}.strac.log"))
      val tracer = new STRACTracer(logFile)
      val i = instantiate(fname, tracer)

      i.interpreter.interpret(2, Vector(), i)
      println(i.module.functions.toList.map(t => t.code.length).sum)

      logFile.flush()
      logFile.close()
      //println(i.exports.function("__original_main").unsafeRunSync())
    }

    test("slumps/test/resources/slumps/bitwise_IO[4_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/resistor_mesh.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[5_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_5_7_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/Banker's_algorithm.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_5_7_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[7_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_4_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/babbage_problem[1].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_7_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_7_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[4].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/aliquot_sequence_classifications_1.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[5_7_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/flipping_bits_game.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_7].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[5_7].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/sqlite3.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_4_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[7].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_5_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[4_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/eban_numbers.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[5_7_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[5].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/babbage_problem[0].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_5_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/babbage_problem.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_5_7].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_4_7_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[4_7_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[4_7].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_4_7].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_4].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[5_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_5].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[2_4_7_9].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[7_10].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/zebra_puzzle.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/flipping_bits_game[0].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/paraffins.wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/babbage_problem[2].wasm") { runAndTrace() }
    test("slumps/test/resources/slumps/bitwise_IO[4_7_10].wasm") { runAndTrace() }
  }

}
