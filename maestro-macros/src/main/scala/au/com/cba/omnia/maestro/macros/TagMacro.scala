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

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.maestro.core.codec.Tag

object TagMacro {
  def impl[A <: ThriftStruct: c.WeakTypeTag](c: Context): c.Expr[Tag[A]] = {
    import c.universe._
    val fields = FieldsMacro.impl[A](c)
    val result = q"""
      import au.com.cba.omnia.maestro.core.codec.Tag
      Tag.fromFields(${fields}.AllFields)
    """
    c.Expr[Tag[A]](result)
  }
}
