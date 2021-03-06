/*
 * Copyright 2020 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package swam
package cli

import runtime.wasi.WasiOption
import enumeratum._
import java.nio.file.Path

import swam.code_analysis.coverage.instrument.InstrumentationType

sealed trait Options

case class Run(file: Path,
               args: List[String],
               main: String,
               wat: Boolean,
               wasi: Boolean,
               time: Boolean,
               trace: Boolean,
               traceFilter: String,
               traceFile: Path,
               dirs: List[Path],
               debug: Boolean,
               wasiOption: WasiOption,
               wasmArgTypes: List[String])
    extends Options

case class Decompile(file: Path, textual: Boolean, out: Option[Path], readChunkSize: Int) extends Options

case class Validate(file: Path, wat: Boolean, dev: Boolean, readChunkSize: Int) extends Options

case class Compile(file: Path, out: Path, debug: Boolean) extends Options

case class WasmCov(file: Path,
                   args: List[String],
                   main: String,
                   wat: Boolean,
                   wasi: Boolean,
                   time: Boolean,
                   trace: Boolean,
                   traceFilter: String,
                   traceFile: Path,
                   dirs: List[Path],
                   debug: Boolean,
                   exportInstrumented: Path,
                   instrumentationType: InstrumentationType,
                   covOut: Path,
                   filter: Boolean,
                   wasmArgTypes: List[String],
                   wasiOption: WasiOption,
                   validate: Boolean)
    extends Options

case class Infer(file: Path, wat: Boolean, main: String) extends Options
