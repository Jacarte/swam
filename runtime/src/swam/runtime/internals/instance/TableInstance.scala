/*
 * Copyright 2018 Lucas Satabin
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
package runtime
package internals
package instance

import scala.language.higherKinds

private[runtime] class TableInstance[F[_]](min: Int, max: Option[Int]) extends Table[F] {

  private val elems = Array.ofDim[Function[F]](min)

  val tpe = TableType(ElemType.AnyFunc, Limits(min, max))

  def size = elems.length

  def apply(idx: Int): Function[F] =
    elems(idx)

  def update(idx: Int, f: Function[F]) =
    elems(idx) = f

}