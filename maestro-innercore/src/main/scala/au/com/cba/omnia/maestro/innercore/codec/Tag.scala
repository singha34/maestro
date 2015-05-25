//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.maestro.innercore.codec

import au.com.cba.omnia.omnitool.Result

import au.com.cba.omnia.maestro.innercore.data.Field

/** Tags a row of cells with the column names for each cell. */
case class Tag[A](run: List[String] => Result[List[(String, Field[A, _])]])

/** [[Tag]] Companion object. */
object Tag {
  /** Creates a [[Tag]] from  a list of [[Field]]. */
  def fromFields[A](fields: => List[Field[A, _]]): Tag[A] = {
    // Keep the field length to avoid having to calculate this for each row.
    val fieldsLength = fields.length

    Tag(row => {
      if (row.length < fieldsLength)
        Result.fail(s"Not enough cells in the row. Got ${row.length} expected ${fields.length}.")
      else if (row.length > fieldsLength)
        Result.fail(s"Too many cells in the row. Got ${row.length} expected ${fields.length}.")
      else
        Result.safe(row zip fields)
    })
  }

  /** Tag the cells in the specified row with its column names. */
  def tag[A : Tag](row: List[String]): Result[List[(String, Field[A, _])]] =
    Tag.of[A].run(row)

  /** Gets the [[Tag]] type class instance for `A`. */
  def of[A : Tag]: Tag[A] =
    implicitly[Tag[A]]
}
