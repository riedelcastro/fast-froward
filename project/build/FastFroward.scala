import sbt._

class FastFroward(info: ProjectInfo) extends ParentProject(info)
{
  lazy val hi = task { println("Yo"); None }
  lazy val core = project("fast-froward-core")
  lazy val examples = project("fast-froward-examples",core)

  val specs = "org.scala-tools.testing" % "specs" % "1.6.0"
  
}
