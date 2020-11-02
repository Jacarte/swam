package swam
package cli_server

import java.nio.file.StandardOpenOption
import java.util.logging.{LogRecord, Formatter => JFormatter}

import cats.effect.{Blocker, ExitCode, IO}
import cats.implicits._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import swam.cli.Main.{prepareFunction, wasiOption, covfilter, debug, dirs, filter, mainFun, restArguments, time, trace, traceFile, wasi, wasmFile, wat}
import swam.code_analysis.coverage.CoverageListener
import swam.cli.utils.inferSignature
import swam.runtime.Engine
import swam.runtime.imports._
import swam.runtime.trace._
import swam.text.Compiler

private object NoTimestampFormatter extends JFormatter {
  override def format(x: LogRecord): String =
    x.getMessage
}

object Main extends CommandIOApp(name = "swam-server-cli", header = "Swam server from the command line") {

  type AsIIO[T] = AsInterface[T, IO]
  type AsIsIO[T] = AsInstance[T, IO]

  ////// CLI-COMMAND ARGUMENT COMBINATIONS //////

    val serverOpts: Opts[Options] =
        Opts.subcommand("run_server", "Run a socket for a given WASM that listens to inputs") {
        // TODO: Check which ones of these are really necessary
        (               mainFun, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasmFile, restArguments, covfilter, wasiOption)
        .mapN { (       main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args, covfilter, wasiOption) =>
            RunServer(  wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug, covfilter, wasiOption)
        }
    }

  val outFileOptions = List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

  def main: Opts[IO[ExitCode]] =
    serverOpts
      .map { opts =>
        Blocker[IO].use { blocker =>
          opts match {
            case RunServer(file,
                           args,
                           main,
                           wat,
                           wasi,
                           time,
                           trace,
                           filter,
                           tracef,
                           dirs,
                           debug,
                           covfilter,
                          wasiOption) =>
              for {
                tracer <- if (trace)
                  JULTracer[IO](blocker,
                                traceFolder = ".",
                                traceNamePattern = tracef.toAbsolutePath().toString(),
                                filter = filter,
                                formatter = NoTimestampFormatter).map(Some(_))
                else
                  IO(None)
                coverageListener = CoverageListener[IO](covfilter)
                engine <- Engine[IO](blocker, tracer, listener = Option(coverageListener))
                tcompiler <- Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
                compiled <- engine.compile(module)
                wasmArgTypes <- inferSignature(compiled, main)
                preparedFunction <- prepareFunction(compiled, wasiOption, main, dirs, args, wasi, blocker)
                _ <- IO(
                  Server
                    .listen(IO(preparedFunction), wasmArgTypes, time, file, coverageListener))
              } yield ExitCode.Success
          }
        }
      }
}
