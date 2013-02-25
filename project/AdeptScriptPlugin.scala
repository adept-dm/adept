import sbt._
import sbt.Keys._

object AdeptScriptPlugin extends Plugin {
  override val projectSettings = super.projectSettings ++ Seq(adeptScript <<= adeptScriptTask)

  lazy val adeptScript = TaskKey[File]("adept-create-script")
  
  lazy val adeptScriptTask = (baseDirectory, fullClasspath in Compile) map { (baseDir, attributedClasspath) =>
    val classpath = attributedClasspath.map(_.data.getAbsolutePath).mkString(":")
    val script = """
#!/bin/bash

classpath="""+classpath+"""

java -cp $classpath adept.cli.Main $*
"""
    val binDir = baseDir / "bin"
    binDir.mkdirs()
    val scriptFile = binDir / "adept"
    IO.write(scriptFile, script)
    scriptFile.setExecutable(true)
    println(scriptFile + " was created! This points to your current build.\nAdd this to your .bashrc to use it: export PATH="+binDir+":$PATH")
    scriptFile
  }


}
