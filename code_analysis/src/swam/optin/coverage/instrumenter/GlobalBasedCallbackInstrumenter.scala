package swam
package code_analysis
package coverage
package instrument

import cats._
import fs2.Stream
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.writePretty
import scodec.Attempt
import swam.binary.custom.{FunctionNames, NameSectionHandler, Names}
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
class GlobalBasedCallbackInstrumenter[F[_]](val coverageMemSize: Int = 1 << 16, val threshold: Int = 0)(
    implicit F: MonadError[F, Throwable])
    extends Instrumenter[F] {

  var previousCount = -1

  def addCallback(ctx: GlobalBasedTransformationContext,
                  map: FunctionTreeMap,
                  addToParent: Boolean = true): Vector[Inst] = {

    val p = previousCount
    previousCount = instructionCount
    if (instructionCount - p >= threshold || p == -1) {
      id += 1
      blockCount += 1

      if (map.children.nonEmpty)
        map.children.last.size = (instructionCount - p)

      val child = new BlockInfo(id)
      map.children = map.children.appended(child)

      Vector(i32.Const(1), GlobalSet(id + ctx.AFLOffset - 1 + ctx.pad))
    } else
      Vector()
  }

  def instrumentVector(instr: Vector[Inst],
                       ctx: GlobalBasedTransformationContext,
                       map: FunctionTreeMap): Vector[Inst] = {

    def instrumentInstruction(i: Inst, idx: Int): Vector[Inst] = {
      i match {
        case Block(tpe, instr) => {
          // Enter to a new map inside the block
          val instrumented = instrumentVector(instr, ctx, map)
          Vector(Block(tpe, instrumented)).concat(addCallback(ctx, map))
        }
        case Loop(tpe, instr) => {
          Vector(Loop(tpe, instrumentVector(instr, ctx, map))).concat(addCallback(ctx, map))
        }
        case If(tpe, thn, els) =>
          val thenBody = instrumentVector(thn, ctx, map)

          val elseBody = instrumentVector(els, ctx, map)
          Vector(If(tpe, thenBody, elseBody))
        case BrIf(lbl) =>
          Vector(BrIf(lbl)).concat(addCallback(ctx, map))
        case x =>
          Vector(x)
      }
    }

    instr.zipWithIndex.flatMap {
      case (ins, i) =>
        instructionCount += 1
        if (i == 0) {
          addCallback(ctx, map, true).concat(instrumentInstruction(ins, i))
        } else
          instrumentInstruction(ins, i)
    }
  }

  implicit val formats = DefaultFormats

  def instrument(sections: Stream[F, Section]): Stream[F, Section] = {
    var map: Vector[FunctionTreeMap] = Vector[FunctionTreeMap]()
    val r = for {
      firstPass <- sections.zipWithIndex
        .fold(GlobalBasedTransformationContext(Seq(), None, None, None, None, None, None, None, None, None, 0, 0, 50)) {
          case (ctx, (c: Section.Types, i)) =>
            ctx.copy(
              types = Option((c, i))
            )
          case (ctx, (c: Section.Elements, i)) => {
            ctx.copy(
              elements = Option((c, i))
            )
          }
          case (ctx, (c: Section.Datas, i)) => {
            ctx.copy(data = Option((c, i)))
          }
          case (ctx, (c: Section.Custom, i)) => // Patch removing custom section
            {
              c match {
                case Section.Custom("name", _) =>
                  ctx.copy(
                    names = Option((c, Int.MaxValue))
                  )
                case _ =>
                  ctx.copy(
                    sections = ctx.sections.appended((c, i))
                  )
              }

            }
          case (ctx, (c: Section.Functions, i)) => {
            ctx.copy(
              functions = Option((c, i))
            )
          }
          case (ctx, (c: Section.Globals, i)) =>
            ctx.copy(
              globals = Option((c, i))
            )
          case (ctx, (c: Section.Imports, i)) => {
            ctx.copy(
              imported = Option((c, i))
            )
          }
          case (ctx, (c: Section.Code, i)) => {
            ctx.copy(
              code = Option((c, i))
            )
          }
          case (ctx, (c: Section.Exports, i)) =>
            ctx.copy(
              exports = Option((c, i))
            )
          case (ctx, (c: Section, i)) =>
            ctx.copy(
              sections = ctx.sections.appended((c, i))
            )
        }

      ctx = firstPass

      ctxExports = ctx.copy(
        names = ctx.names,
        AFLOffset = {
          ctx.globals match {
            case Some(g) => g._1.globals.length + 1
            case None    => 0
          }
        }
      )

      wrappingCode = ctxExports.copy(
        code = Option(
          (Section.Code(
             ctxExports.code.get._1.bodies.zipWithIndex.map(f => {
               val child = new FunctionTreeMap(Vector[BlockInfo](), desc = s"f${f._2}")
               map = map.appended(child)
               FuncBody(f._1.locals, instrumentVector(f._1.code, ctxExports, child))
             })
           ),
           ctxExports.code.get._2)),
        exports = Option(
          Section.Exports( // TODO patch with mnatch option in case the section does not exist
            ctxExports.exports.get._1.exports.concat(
              Range(0, blockCount).map(i =>
                Export(s"cg${i + ctxExports.AFLOffset + ctxExports.pad}",
                       ExternalKind.Global,
                       i + ctxExports.AFLOffset + ctxExports.pad))
            )),
          ctxExports.exports.get._2
        ),
        blockCount = blockCount
      )

      dataCtx = wrappingCode.copy(
        globals = Option(
          Section.Globals(
            (wrappingCode.globals match {
              case Some(g) =>
                g._1.globals
              case None =>
                Vector()
            }).concat(
                Range(0, wrappingCode.pad + 1).map(_ => Global(GlobalType(ValType.I32, Mut.Var), Vector(i32.Const(0))))
              ) // Padding control
              .concat(Range(0, blockCount).map(_ => Global(GlobalType(ValType.I32, Mut.Var), Vector(i32.Const(0)))))
          ),
          wrappingCode.globals.get._2
        ),
        data = Option(
          Section.Datas(
            ({
              wrappingCode.data match {
                case Some(realData) =>
                  realData._1.data
                case None => Vector()
              }
            }) /*.appended(Data(
                0, // In the current version of WebAssembly, at most one memory is allowed in a module. Consequently, the only valid memidxmemidx is 00.
                Vector(i32.Const(wrappingCode.AFLOffset)),
                BitVector(new Array[Byte](coverageMemSize))
              ))
              .appended(Data(
                0, // In the current version of WebAssembly, at most one memory is allowed in a module. Consequently, the only valid memidxmemidx is 00.
                Vector(i32.Const(coverageMemSize + wrappingCode.AFLOffset)),
                BitVector(new Array[Byte](blockCount))
              ))*/
            // BLOCK Coverage memory piece
          ),
          wrappingCode.data match {
            case Some(realData) => realData._2
            case None           => 11
          }
        ),
        code = Option(
          Section.Code(wrappingCode.code.get._1.bodies),
          wrappingCode.code.get._2
        )
      )

    } yield dataCtx

    //r.map(t => Stream.emits(t.sections))
    r.flatMap(t => {

      // Output JSON with the metadata
      println(
        writePretty(
          new CoverageMetadaDTO(
            instructionCount,
            blockCount,
            2,
            t.AFLOffset + t.pad,
            -1,
            t.pad,
            map = map.sortBy(t => -1 * t.children.length)
          )))

      //System.err.println(s"Number of instrumented blocks $blockCount. Number of instructions $instructionCount")

      Stream.emits(t.sortedSections)
    })
  }
}
