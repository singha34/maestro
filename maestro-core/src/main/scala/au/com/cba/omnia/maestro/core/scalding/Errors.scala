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

package au.com.cba.omnia.maestro.core.scalding

import cascading.flow.FlowDef
import cascading.pipe.Pipe

import com.twitter.scalding.{FieldConversions, Source, TypedPipe, TypedPsv}
import com.twitter.scalding.typed.TypedSink

import scalaz.{\/, -\/, \/-}

object Errors extends FieldConversions {

  /*
   * This is totally unacceptable but is the only mechanism by which we have
   * been able to construct error handling for validation on sources that can
   * be written to file and doesn't involve doing multiple passes on the
   * source data.
   *
   * It is also _dangerous_ in that you can not add multiple traps. If you
   * do, you will receive a _runtime_ error (please do not swallow this,
   * fix your program so it only has one trap).
   *
   * The process is basically that we give up on types and hack away:
   *   - Go into untyped land by stripping off the "left" data constructor (this
   *     means we end up with a TypedPipe[Any]) that will either have raw failure
   *     strings, or success in a "right" data constructor. We do this so errors
   *     are written out raw, without the \/-() constructor.
   *   - Make the conversion strict via a call to fork.
   *   - Add a trap on the regular pipe, for routing all failures to an error source.
   *   - Force errors into error pipe, by:
   *     - Doing a runtime match on rights, and returning the raw value.
   *     - For all other cases (i.e. our errors), throw an exception to trigger
   *       the trap. The trap writes out the value in the pipe, not the exception,
   *       but should neatly write out the error because of our previous hack to
   *       strip off the left constructor.
   */
  def handle[A](p: TypedPipe[String \/ A], errors: Source with TypedSink[Any]): TypedPipe[A] =
    p
      .flatMap({
        case     -\/(error) => List(error)
        case v @ \/-(value) => List(v)
      })
      .fork
      .addTrap(errors)
      .map({
        case \/-(value) => value.asInstanceOf[A]
        case v          => sys.error("trap: The real error was: " + v.toString)
      })

  def safely[A](path: String)(p: TypedPipe[String \/ A]): TypedPipe[A] =
    handle(p, TypedPsv(path))
}
