import mill._
import mill.scalalib._

object arse extends ScalaModule {
  def scalaVersion = "2.13.6"
  def publishVersion = "0.2.0"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.2.7")
}
