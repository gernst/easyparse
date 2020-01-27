// ARSE Parser libary
// (c) 2020 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

class Input(val text: String, var position: Int, val whitespace: Whitespace) {
  def length = text.length - position
  def rest = text drop position
  def isEmpty = (length == 0)

  def advanceBy(offset: Int) {
    advanceTo(position + offset)
  }

  def advanceTo(next: Int) {
    whitespace.matches(text, next) match {
      case Some(next) =>
        position = whitespace.matcher.end
      case None =>
        position = next
    }
  }
  
  override def toString = rest
}
