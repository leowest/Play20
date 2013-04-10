import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

    val appName         = "forms"
    val appVersion      = "1.0"

    val appDependencies = Seq(
      // Add your project dependencies here,
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      scalaVersion := "2.10.1",
      scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")
    )

}
