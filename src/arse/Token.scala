package arse

case class Location(source: String, range: Range)

case class Token(text: String, location: Location) {
  override def toString = text
}