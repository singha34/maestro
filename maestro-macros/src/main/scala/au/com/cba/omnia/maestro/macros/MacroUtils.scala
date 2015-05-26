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

package au.com.cba.omnia.maestro.macros

import scala.reflect.macros.whitebox.Context
import scala.util.{Try, Failure, Success}

object MacroUtils {
  /** Returns `Some(t)` iff `o` is `Option[T]` else returns `None`. */
  def optional(c: Context)(o: c.Type): Option[c.Type] = {
    import c.universe._

    val optionConstructor = weakTypeOf[Option[_]].typeConstructor

    if (o.typeConstructor == optionConstructor) {
      val TypeRef(_, _, typParams) = o
      Some(typParams.head)
    } else None
  }

  /**
   * Attempts to compile `code` and returns a sequence of error messages. The messages are calculated at compile time but
   * returned at runtime. Inspired by :
   *
   * [[https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/test/typechecking.scala]]
   *
   * @param code the string literal containing the code to compile. NOTE: This must be a string literal, not an arbitrary
   *             String - typed value, since the actual check is done at compile time.
   * @return a sequence of error messages, or an empty sequence if `code` compiles correctly.
   */
  def compileErrors(code: String): Option[String] = macro compileErrorsImpl

  def compileErrorsImpl(c:Context)(code: c.Expr[String]):c.Expr[Option[String]] = {
    import c.universe.{Try => _, _}

    val Expr(Literal(Constant(codeStr: String))) = code
    val attemptToTypecheck = Try(c.typecheck(c.parse(s"{ val ${TermName(c.freshName)} = { $codeStr } }")))


    val errors: Option[String] = attemptToTypecheck match {
      case Failure(e) => Some(e.getMessage)
      case Success(_) => None
    }
    c.Expr[Option[String]](q"${errors}")
  }

}
