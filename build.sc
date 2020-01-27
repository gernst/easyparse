import mill._
import mill.scalalib._

object arse extends ScalaModule {
  def scalaVersion = "2.12.8"
  def publishVersion = "0.1.3"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.7")
}
