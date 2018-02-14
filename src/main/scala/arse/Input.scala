package arse

class Input(val text: String, var position: Int, var commit: Boolean, val whitespace: Whitespace) {
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
    commit = true
  }
}