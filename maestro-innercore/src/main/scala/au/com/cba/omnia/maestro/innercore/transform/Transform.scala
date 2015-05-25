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

package au.com.cba.omnia.maestro.innercore.transform

/**
  * By convention, a function between thrift structures.
  *
  * [[au.com.cba.omnia.maestro.macros.Macros.mkTransform]] will automatically
  * generate [[Transform]]s when `A` and `B` are thrift structs, by matching
  * their fields, or failing compilation if that is not possible.
  */
case class Transform[A, B](run: A => B)

/**
  * By convention, a function that joins a tuple of thrift structures to produce
  * a new thrift structure.
  *
  * [[au.com.cba.omnia.maestro.macros.Macros.mkJoin]] will automatically
  * generate [[Join]]s when `A` is a product of thrift structs and `B` is a
  * thrift struct, by matching fields between the output and input structs, or
  * failing compilation if that is not possible.
  */
case class Join[A, B](run: A => B)
