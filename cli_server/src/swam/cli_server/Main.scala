package swam
package cli_server

import java.nio.file.{Path, Paths, StandardOpenOption}
import java.util.logging.{LogRecord, Formatter => JFormatter}

import cats.effect.{Blocker, ExitCode, IO}
import cats.implicits._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import swam.cli.Main.prepareFunction
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

  ////// CLI-COMMAND ARGUMENTS //////
  // Beware (Opts.argument != Opts.arguments) && (Opts.option != Opts.options) && (Opts.flag != Opts.flags)

  val wasmFile =
    Opts.argument[Path](metavar = "wasm")

  val func_name =
    Opts.argument[String](metavar = "functionName")

  // Arguments that get passed to the WASM code you execute. They are available through WASI args_get.
  val restArguments =
    Opts.arguments[String](metavar = "args").orEmpty

  val wasmArgTypes = Opts
    .options[String]("argType",
      help = "Input parameter types for Wasm function. Possible values: Int32, Int64, Float32, Float64.",
      short = "aT")
    .orEmpty

  val dirs = Opts
    .options[Path]("dir", "Preopen directory", short = "D", metavar = "dir")
    .orEmpty

  val mainFun =
    Opts
      .option[String]("main", "Execute function of provided name (default is _start)", short = "m")
      .withDefault("_start")

  val wat =
    Opts.flag("wat", "Input file is in WAT format, and needs to be parsed as such (default false)", short = "w").orFalse

  val wasi =
    Opts.flag("wasi", "Program is using wasi (default false)", short = "W").orFalse

  val time =
    Opts.flag("time", "Measure execution time (default false)", short = "C").orFalse

  val trace =
    Opts.flag("trace", "Trace WASM execution channels (default false)", short = "t").orFalse

  /*val cov =
    Opts.flag("instcov", "Run the WebAssembly module and gets coverage.", short = "v").orFalse*/

  val covfilter = Opts.flag("cov-filter", "Generate coverage with filter on Wasi Methods", short = "r").orFalse

  val covOut = Opts
    .option[Path]("covout", "Output folder for coverage reports and show-map", short = "c")
    .withDefault(Paths.get(".").toAbsolutePath.normalize)

  val filter =
    Opts
      .option[String](
        "filter",
        "Filter the traces. The parameter is a regular expression applied to the opcode, e.g.: 'mread|mwrite' (default *)",
        short = "f")
      .withDefault("*")

  val traceFile =
    Opts
      .option[Path](
        "trace-file",
        "The file to which traces are written (default trace.log)",
        short = "l"
      )
      .withDefault(Paths.get("trace.log"))

  val debug =
    Opts.flag("debug", "Generate debug elements when compiling wat format (default false)", short = "d").orFalse

  val out = Opts
    .option[Path]("out", "Save decompiled result in the given file. Prints to stdout if not provider", short = "o")

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
