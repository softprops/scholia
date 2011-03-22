import sbt._
class ScholiaProject(info: ProjectInfo) extends DefaultProject(info) {
  val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"
  val knockoff = "com.tristanhunt" %% "knockoff" % "0.7.3-15"
}
