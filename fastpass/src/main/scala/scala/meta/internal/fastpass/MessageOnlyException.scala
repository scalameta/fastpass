package scala.meta.internal.fastpass

import scala.util.control.NoStackTrace

case class MessageOnlyException(message: String)
    extends Exception(message)
    with NoStackTrace
