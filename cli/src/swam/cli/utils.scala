package swam
package cli

import java.util.concurrent.TimeUnit
import cats.effect._
import cats._
import cats.implicits._
import io.odin.Logger
import swam.ValType.{F32, F64, I32, I64}
import swam.binary.custom.FunctionNames
import swam.runtime.{Module, Value}

/**
  * @author Javier Cabrera-Arteaga on 2020-11-02
  */
object utils {

  // TODO move these methods to code_analysis module
  // Create the required input vector for the instantiated Wasm function
  def parseWasmArgs(argsTypes: List[String], args: List[String]): Vector[Value] = {
    if (argsTypes.length != args.length)
      throw new Exception("Number of args not equal to number of arg types!")
    argsTypes.zipWithIndex.map {
      case ("Int32", index) =>
        Value.Int32(args(index).toInt)
      case ("Int64", index) =>
        Value.Int64(args(index).toLong)
      case ("Float32", index) =>
        Value.Float32(args(index).toFloat)
      case ("Float64", index) =>
        Value.Float64(args(index).toDouble)
      case (unknownType, _) =>
        throw new Exception("Type does not exist for Wasm: " + unknownType)
    }.toVector
  }

  def inferSignature(compiled: Module[IO], funcName: String): IO[Vector[ValType]] = {
    for {
      allFuncsOpt <- IO(compiled.names.flatMap(_.subsections.collectFirst { case FunctionNames(n) => n }))
      signature <- IO(
        allFuncsOpt match {
          case None => throw new Exception("The module does not contain a name/metadata section")
          case Some(allFuncs) => {
            val matchingFuncs = allFuncs.filter { case (_, name) => funcName == name }
            if (matchingFuncs.isEmpty) {
              System.err.println(s"Function '$funcName' does not exist. Listing available functions...")
              val functionsAvailable = allFuncs.map { case (_, value) => value }.mkString("\t")
              throw new Exception(s"Function '$funcName' does not exist. Available functions: $functionsAvailable")
            } else {
              if (matchingFuncs.size > 1) {
                System.err.println(s"Warning $funcName has more than one definition, taking the first one")
              }
              val funcIdx = matchingFuncs.collectFirst { case (tid, _) => tid }.get

              // There is always one at this point
              val funcType = compiled.functions.filter(f => f.idx == funcIdx)(0).tpe

              funcType.params
            }
          }
        }
      )
    } yield signature
  }
  // Create the required input vector for the instantiated Wasm function
  def createInputVector(argsTypes: Vector[ValType], args: List[String]): Vector[Value] = {
    if (argsTypes.length != args.length)
      throw new Exception("Number of args not equal to number of arg types!")
    argsTypes.zipWithIndex.map {
      case (I32, index) =>
        Value.Int32(args(index).toInt)
      case (I64, index) =>
        Value.Int64(args(index).toLong)
      case (F32, index) =>
        Value.Float32(args(index).toFloat)
      case (F64, index) =>
        Value.Float64(args(index).toDouble)
    }
  }
}
