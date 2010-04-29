import sbt._

class FastFrowardExamples(info: ProjectInfo) extends DefaultProject(info)
{
  lazy val hi = task { println("Peace"); None }

}
