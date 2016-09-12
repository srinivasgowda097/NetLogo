// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim

import org.nlogo.core.{ Let, Syntax }
import org.nlogo.nvm.{ Command, Context, MutableLong }

class _repeatlocalinternal(private[this] val _vn: Int, _offset: Int) extends Command {
  offset = _offset

  override def toString: String =
    return super.toString + ":" + offset + "," + _vn

  override def perform(context: Context) {
    val counter = context.activation.args(_vn).asInstanceOf[MutableLong]
    if (counter.value <= 0) {
      context.ip = next
    } else {
      counter.value = counter.value - 1
      context.ip = offset
    }
  }

  def perform_1(context: Context) {
    val counter = context.activation.args(_vn).asInstanceOf[MutableLong]
    if (counter.value <= 0) {
      context.ip = next
    } else {
      counter.value = counter.value - 1
      context.ip = offset
    }
  }
}
