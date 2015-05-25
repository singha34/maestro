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

package au.com.cba.omnia.maestro.test

import scala.collection.JavaConverters._

import scalaz.effect.IO

import com.google.common.base.Splitter

import au.com.cba.omnia.thermometer.context.Context
import au.com.cba.omnia.thermometer.core.ThermometerRecordReader

import au.com.cba.omnia.maestro.innercore.codec.{Decode, DecodeOk, DecodeError}

trait Records {
  /**
   * Thermometer record reader for reading records from delimited text.
   * If the record `A` is a `ThriftStruct`, you can get the decoder by `Macros.mkDecode[A]`
   **/
  def delimitedThermometerRecordReader[A](delimiter: Char, none: String, decoder: Decode[A]) =
    ThermometerRecordReader((conf, path) => IO {
      val splitter = Splitter.on(delimiter)

      new Context(conf).lines(path).map(l =>
        decoder.decode(none, splitter.split(l).asScala.toList) match {
          case DecodeOk(c)       => c
          case e: DecodeError[A] => throw new Exception(s"Can't decode line $l in $path: $e")
        })
    })
}
