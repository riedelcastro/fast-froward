import java.io.PrintStream
import sbt._

class FastFrowardCore(info: ProjectInfo) extends DefaultProject(info)
{
  lazy val hi = task {println("Peace"); None}

  val specs = "org.scala-tools.testing" % "specs" % "1.6.0"
  val junit = "junit" % "junit" % "4.4"

  lazy val ff = consoleTask(consoleClasspath,
    "import org.riedelcastro.thebeast.env._\n" +
            "import org.riedelcastro.thebeast.solve._\n" +
            "import org.riedelcastro.thebeast.env.doubles._\n" +
            "import org.riedelcastro.thebeast.env.booleans._\n" +
            "import org.riedelcastro.thebeast.env.TheBeastImplicits._") describedAs "fast-froward interpreter"

  lazy val generateScript = task {
    var fileName = "%s/ff".format(outputPath.relativeString)
    log.info("Creating script %s".format(fileName))
    val file = new PrintStream(fileName)
    file.println(ffScript.format(runClasspath.absString))
    file.close
    None
  }


  private val ffScript =
  """ #!/bin/bash
      CLASSPATH=%s

      until [ -z "$1" ]; do
         case $1 in
             -classpath|-cp)
                 shift
                 CLASSPATH="${CLASSPATH}:${1}"
             ;;
             * )
                 ARGS="${ARGS} $1"
             ;;
         esac
         shift
      done

      echo $CLASSPATH

      # To execute
      if [ -n "$CLASSPATH" ]
      then
         scala -classpath ${CLASSPATH} ${ARGS}
      else
         scala ${ARGS}
      fi
  """

}
