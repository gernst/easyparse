import mill._
import mill.scalalib._
import mill.scalalib.publish._

object arse extends ScalaModule with PublishModule {
  def scalaVersion = "2.12.8"
  def publishVersion = "0.1.2"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.7")

  def pomSettings = PomSettings(
    description = "Automatic Recovery of Syntactic Elements (ARSE)",
    organization = "de.gidonernst",
    url = "https://github.com/gernst/arse",
    licenses = Seq(License.MIT),
    scm = SCM(
      "git://github.com/gernst/arse.git",
      "scm:git://github.com/gernst/arse.git"
      ),
    developers = Seq(
      Developer("gernst", "Gidon Ernst", "https://github.com/gernst")
      )
    )
}
