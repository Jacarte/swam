package swam
package cli_server

import java.nio.file.{Path, Paths, StandardOpenOption}
import java.util.logging.{LogRecord, Formatter => JFormatter}

import cats.effect.{Blocker, ExitCode, IO}
import cats.implicits._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import swam.cli.Main.{
  prepareFunction,
  wasmFile,
  func_name,
  restArguments,
  wasmArgTypes,
  dirs,
  mainFun,
  wat,
  wasi,
  time,
  trace,
  covfilter,
  covOut,
  filter,
  traceFile,
  debug,
  out
}
import swam.code_analysis.coverage.CoverageListener
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
      (mainFun,
       wat,
       wasi,
       time,
       dirs,
       trace,
       traceFile,
       filter,
       debug,
       wasmFile,
       restArguments,
       covfilter,
       wasmArgTypes)
        .mapN { (main, wat, wasi, time, dirs, trace, traceFile, filter, debug, wasm, args, covfilter, wasmArgTypes) =>
          RunServer(wasm, args, main, wat, wasi, time, trace, filter, traceFile, dirs, debug, covfilter, wasmArgTypes)
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
                           wasmArgTypes) =>
              for {
                tracer <- if (trace)
                  JULTracer[IO](blocker,
                                traceFolder = ".",
                                traceNamePattern = tracef.toAbsolutePath().toString(),
                                filter = filter,
                                formatter = NoTimestampFormatter).map(Some(_))
                else
                  IO(None)
                // TODO: Use swam.cli.Main.inferSignature here to get wasmArgTypes
                coverageListener = CoverageListener[IO](covfilter)
                engine <- Engine[IO](blocker, tracer, listener = Option(coverageListener))
                tcompiler <- Compiler[IO](blocker)
                module = if (wat) tcompiler.stream(file, debug, blocker) else engine.sections(file, blocker)
                compiled <- engine.compile(module)
                preparedFunction <- prepareFunction(compiled, main, dirs, args, wasi, blocker)
                _ <- IO(
                  Server
                    .listen(IO(preparedFunction), wasmArgTypes, time, file, coverageListener))
              } yield ExitCode.Success
          }
        }
      }
}
