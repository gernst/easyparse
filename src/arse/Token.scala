// ARSE Parser libary
// (c) 2015 Gidon Ernst
// This code is licensed under MIT license (see LICENSE for details)

package arse

case class Location(source: String, range: Range)

case class Token(text: String, location: Location) {
  override def toString = text
}
