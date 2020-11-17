package swam
package code_analysis
package coverage
package instrument

import cats._
import fs2.Stream
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.writePretty
import swam.code_analysis.coverage.utils.{
  FunctionTreeMap,
  CoverageMetadaDTO,
  GlobalBasedTransformationContext,
  BlockInfo
}
import swam.syntax._

/**
  * @author Javier Cabrera-Arteaga on 2020-10-16
  */
class ExtendedGlobalBasedCallbackInstrumenter[F[_]](
    override val coverageMemSize: Int = 1 << 16,
    override val threshold: Int = 2)(implicit F: MonadError[F, Throwable])
    extends GlobalBasedCallbackInstrumenter[F](coverageMemSize, threshold) {

  override def addCallback(ctx: GlobalBasedTransformationContext,
                           map: FunctionTreeMap,
                           addToParent: Boolean = true): Vector[Inst] = {

    val p = previousCount
    previousCount = instructionCount
    val currentBlockGlobalIndex = ctx.AFLOffset - 1 + ctx.pad
    if (instructionCount - p >= threshold || p == -1) {
      id += 1
      blockCount += 1

      Vector(GlobalGet(currentBlockGlobalIndex),
             GlobalSet(id + ctx.AFLOffset - 1 + ctx.pad),
             i32.Const(id),
             GlobalSet(currentBlockGlobalIndex))
    } else
      Vector()
  }

}
