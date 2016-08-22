package arse

case class Token(text: String, start: Int) {
  def length = text.length
  def end = start + length
}