package swam
package optin
package coverage

import swam.runtime.internals.interpreter.{AsmInst, Continuation, Frame, InstructionListener,InstructionWrapper}
import cats.effect.{Async, ContextShift}
import cats._
import java.io._


/**
  * @author Javier Cabrera-Arteaga on 2020-06-11
  */
class CoverageListener[F[_]: Async](wasi:Boolean) extends InstructionListener[F] {
  var coverageMap = Map[(Int, String), (String, AsmInst[F], Int)]()

  override val wasiCheck : Boolean = wasi

  override def before(inner: InstructionWrapper[F], index: Int,frame: Frame[F]): Unit = {}

  override def after(inner: InstructionWrapper[F], index: Int, frame: Frame[F], functionName:Option[String],result: Continuation[F]): Continuation[F] = {
    val fn:String = functionName.getOrElse("N/A").toString
    val prev = coverageMap((index,fn))

    val count:Option[(String,AsmInst[F],Int)] = coverageMap.get((index, fn))
    if (count == null) {
      coverageMap = coverageMap.updated((index, fn), (fn, prev._2, 1))
    }
    else{
      val i = (count match {
        case Some(x) => x._3 + 1 // this extracts the value in a as an Int
        case _ => 1
        })
      coverageMap = coverageMap.updated((index, fn), (fn, prev._2, i))
    }
    //println(coverageMap)
    result
  }

  override def init(inner: InstructionWrapper[F], index: Int, functionName: Option[String]): Unit = {
    val fn = functionName.getOrElse("N/A").toString
    
    coverageMap = coverageMap.updated((index, fn), (fn, inner, 0))

  }
}

object CoverageListener {
  def apply[F[_]: Async: ContextShift](wasi:Boolean): CoverageListener[F] = {

    new CoverageListener[F](wasi)

  }
}