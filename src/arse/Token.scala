// ARSE Parser libary
// (c) 2016 Gidon Ernst <gidonernst@gmail.com>
// This code is licensed under MIT license (see LICENSE for details)

package arse

case class Location(source: String, range: Range) {
  def this(source: String, start: Int, end: Int) = this(source, start until end)
  def ++(that: Location) = {
    assert(this.source == that.source)
    Location(source, this.range.start until that.range.end)
  }
}

case class Token(text: String, location: Location) {
  override def toString = text
}
