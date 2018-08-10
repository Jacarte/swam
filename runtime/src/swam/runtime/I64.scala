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

import java.lang.{Double => JDouble}

object I64 {

  def extendSi32(i: Int): Long =
    i.toLong

  def extendUi32(i: Int): Long =
    i & 0x00000000ffffffffl

  def truncSf32(f: Float): Long =
    if (f.isNaN)
      throw new ConversionException("invalid conversion to integer")
    else if (f >= -Long.MinValue.toFloat || f < Long.MinValue.toFloat)
      throw new ConversionException("integer overflow")
    else
      f.toLong

  def truncUf32(f: Float): Long =
    if (f.isNaN)
      throw new ConversionException("invalid conversion to integer")
    else if (f >= -Long.MinValue.toDouble * 2.0d || f <= -1.0d)
      throw new ConversionException("integer overflow")
    else if (f >= -Long.MinValue.toDouble)
      (f - 9223372036854775808.0f).toLong | Long.MinValue
    else
      f.toLong

  def truncSf64(d: Double): Long =
    if (d.isNaN)
      throw new ConversionException("invalid conversion to integer")
    else if (d >= -Long.MinValue.toDouble || d < Long.MinValue.toDouble)
      throw new ConversionException("integer overflow")
    else
      d.toLong

  def truncUf64(d: Double): Long =
    if (d.isNaN)
      throw new ConversionException("invalid conversion to integer")
    else if (d >= -Long.MinValue.toDouble * 2.0d || d <= -1.0d)
      throw new ConversionException("integer overflow")
    else if (d >= -Long.MinValue.toDouble)
      (d - 9223372036854775808.0d).toLong | Long.MinValue
    else
      d.toLong

  def reinterpret(d: Double): Long =
    JDouble.doubleToRawLongBits(d)

}
