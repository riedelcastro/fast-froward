import sbt._

class FastFrowardCore(info: ProjectInfo) extends DefaultProject(info)
{
  lazy val hi = task { println("Peace"); None }

  val specs = "org.scala-tools.testing" % "specs" % "1.6.0"
  val junit = "junit" % "junit" % "4.4"
  
}
