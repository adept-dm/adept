
package adept.bintray

import sbt.{File => _, URL => _, _}
import sbt.Keys._

import java.net._
import java.io._

case class BintrayVersion(packageName: String, version: String) {
  override lazy val toString = {
    packageName + "/" + version
  }
}

case class BintrayInfo(subject: String, apiKey: String, repo: String) {
  override lazy val toString = {
    subject + "/" + repo
  }
}

trait HttpHelpers { //do not want dependencies so use handwritten ones instead
  import scala.collection.mutable.ArrayBuilder

  private val BufferSize = 4096

  def withBasicAuth(username: String, password: String): Map[String, String] = {
    val encoded = new sun.misc.BASE64Encoder().encode((username + ":" + password).getBytes)
    Map("Authorization" -> ("Basic " + encoded.replaceAll("\n", "")))
  }

  def withContentType(contentType: String, charSet: String = "utf-8") = {
    Map("Content-Type" -> (contentType + ";  charset=" + charSet))
  }

  def fileWriter(file: File)(writer: DataOutputStream) = {
    val reader = new FileInputStream(file)
    try {
      val bytes = Array.fill[Byte](BufferSize)(0)
      var bytesRead = reader.read(bytes)
      var msg = ""
      var sumBytes = 0
      while (bytesRead != -1) {
        val writeBytes = Array.ofDim[Byte](bytesRead)
        Array.copy(bytes, 0, writeBytes, 0, bytesRead)
        print(msg)
        writer.write(writeBytes)
        print("\r" * msg.size)
        sumBytes += bytesRead
        msg = "wrote: " + sumBytes
        bytesRead = reader.read(bytes)
      }
    } finally {
      reader.close()
    }
  }

  def stringWriter(string: String)(writer: DataOutputStream) = {
    writer.writeBytes(string)
  }

  def readBytes(inputStream: InputStream): Array[Byte] = {
    var reader: DataInputStream = null
    try {
      reader = new DataInputStream(inputStream)
      val bytesBuilder = ArrayBuilder.make[Byte]
      val bytes = Array.fill[Byte](BufferSize)(0)

      var bytesRead = reader.read(bytes)
      while (bytesRead != -1) {
        val writeBytes = Array.ofDim[Byte](bytesRead)
        Array.copy(bytes, 0, writeBytes, 0, bytesRead)
        bytesBuilder ++= writeBytes
        bytesRead = reader.read(bytes)
      }
      bytesBuilder.result()
    } finally {
      if (reader != null) reader.close()
    }
  }

  def httpRequest(url: String, requestMethod: String, requestProperties: Map[String, String], writerFun: (DataOutputStream => Unit)*) = {
    new URL(url).openConnection() match {
      case connection: HttpURLConnection =>
        var writer: DataOutputStream = null
        try {
          connection.setDoInput(true)
          connection.setRequestMethod(requestMethod)
          requestProperties.foreach {
            case (key, value) =>
              connection.setRequestProperty(key, value)
          }
          writerFun.foreach { w =>
            connection.setDoOutput(true)
            writer = new DataOutputStream(connection.getOutputStream)
            w(writer)
            writer.flush()
          }
          val result = readBytes(connection.getInputStream)

          (connection.getResponseCode(), connection.getResponseMessage(), result)
        } catch {
          case e: java.io.IOException =>
            val error = readBytes(connection.getErrorStream()) //try again to get the error, if there is none, we throw again here
            (connection.getResponseCode(), connection.getResponseMessage(), error)
        } finally {
          if (writer != null) writer.close()
        }
    }
  }
}

object Bintray extends HttpHelpers {
  val jsonContentType = "application/json"

  val Bintray = "https://bintray.com/api/v1"

  def repos(username: String, apiKey: String) = {
    httpRequest(Bintray + "/repos/" + username, "GET",
      withBasicAuth(username, apiKey))
  }
  val CharSet = "UTF-8"

  private type HttpResult = Either[(Int, String, Array[Byte]), (Int, String, Array[Byte])]
  private val AsString = { res: (Int, String, Array[Byte]) => (res._1, res._2, new String(res._3, CharSet)) }
  
  private def stringify(res: HttpResult) = {
    res.right.map(AsString).left.map(AsString)
  }

  private def accept(code: Int, url: String, res: (Int, String, Array[Byte])): HttpResult = {
    res match {
      case (`code`, msg, result) => Right((code, msg, result))
      case (code, msg, result) => Left((code, msg, result))
    }
  }

  def createVersion(info: BintrayInfo, version: BintrayVersion) = {
    val url = Bintray + "/packages/" + info.subject + "/" + info.repo + "/" + version.packageName + "/versions"
    val json = """{
            "name": """" + version.version + """"
          }""".stripMargin
    val res = httpRequest(url, "POST",
      withBasicAuth(info.subject, info.apiKey) ++ withContentType(jsonContentType, CharSet),
      stringWriter(json))
    stringify(accept(201, url, res))
  }

  def uploadFile(info: BintrayInfo, version: BintrayVersion, file: File, path: String) = {
    val url = Bintray + "/content/" + info.subject + "/" + info.repo + "/" + path + ";bt_package=" + version.packageName + ";bt_version=" + version.version + ";publish=0;explode=0]"
    val res = httpRequest(url, "PUT",
      withBasicAuth(info.subject, info.apiKey) ++ withContentType(jsonContentType, CharSet),
      fileWriter(file))
    stringify(accept(201, url, res))
  }
  def deleteVersion(info: BintrayInfo, version: BintrayVersion) = {
    val url = Bintray + "/packages/" + info.subject + "/" + info.repo + "/" + version.packageName + "/versions/" + version.version
    val res = httpRequest(url, "DELETE",
      withBasicAuth(info.subject, info.apiKey) ++ withContentType(jsonContentType, CharSet))
    stringify(accept(200, url, res))
  }

  def getPackage(info: BintrayInfo, version: BintrayVersion) = {
    val url = Bintray + "/packages/" + info.subject + "/" + info.repo + "/" + version.packageName
    val res = httpRequest(url, "GET",
      withBasicAuth(info.subject, info.apiKey) ++ withContentType(jsonContentType, CharSet))
    stringify(accept(200, url, res))
  }

  def createPackage(info: BintrayInfo, version: BintrayVersion, licenses: Set[String]) = {
    val url = Bintray + "/packages/" + info.subject + "/" + info.repo
    val json = """{
            "name": """" + version.packageName + """",
            "licenses": """ + licenses.mkString("[\"","\",\"", "\"]") + """
          }""".stripMargin
    val res = httpRequest(url, "POST",
      withBasicAuth(info.subject, info.apiKey) ++ withContentType(jsonContentType, CharSet),
      stringWriter(json))
    stringify(accept(201, url, res))
  }
}

object BintrayKeys {
  lazy val bintrayIvyPublish = TaskKey[Seq[String]]("bintray-ivy-publish", "publish a version")
  lazy val bintrayInfo = TaskKey[BintrayInfo]("bintray-info", "the information needed to create a version")
  lazy val bintrayDeleteFirst = TaskKey[Boolean]("bintray-delete-first", "if this is set, try to clean up/delete version first")
}

object BintrayRelease extends Plugin {
  import adept.bintray.BintrayKeys._

  override val projectSettings = super.projectSettings ++ Seq(
    bintrayIvyPublish <<= bintrayIvyPublishTask,
    bintrayDeleteFirst := true
  )

  def majorVersion(version: String) = {
    version.split("\\.").slice(0, 2).mkString(".") //basical: 2.10.1 => 2.10
  }

  def transformScalaVersion(scalaVersion: String) = {
    if (scalaVersion.startsWith("2.10")) majorVersion(scalaVersion)
    else scalaVersion
  }
  

  lazy val bintrayIvyPublishTask = (sbtPlugin, organization, name, version, sbtVersion, scalaVersion, deliverLocal, packagedArtifacts, bintrayInfo, bintrayDeleteFirst, streams) map { (sbtPlugin, organization, name, version, sbtVersion, scalaVersion, ivyFile, packagedArtifacts, info, deleteFirst, s) =>
    import adept.bintray.Bintray._
    val bintrayVersion = BintrayVersion(name, version)

    if (deleteFirst) {
      s.log.warn("deleting: " + info)
      deleteVersion(info, bintrayVersion).left.foreach{ case (c, m, r) =>
        s.log.warn("while deleting version: " +info + " got: " + c + "(" + m + "): " + r)
      }
    }
    val a = getPackage(info, bintrayVersion)
    if (a.isLeft) {
      createPackage(info, bintrayVersion, Set("Apache-2.0")).left.foreach{ case (c, m, r) =>
        s.log.warn("while creating package: " +info + " got: " + c + "(" + m + "): " + r)
      }
    }

    createVersion(info, bintrayVersion).left.foreach{ case (c, m, r) =>
        s.log.warn("while creating version: " +info + " got: " + c + "(" + m + "): " + r)
    }

    val artifacts = packagedArtifacts.groupBy{ case (a, f) => a.`type` }

    val fixedScalaVersion = transformScalaVersion(scalaVersion)
    val fixedSbtVersion = majorVersion(sbtVersion)

    def upload(file: File, path: String) = {
      val res = uploadFile(info, bintrayVersion, file, path)
      res.left.foreach{ case (c, m, r) =>
          s.log.warn("while uploading file: " + file + " to " + path + " got: " + c + "(" + m + "): " + r)
      }
      res.right.foreach{ case (c, m, r) =>
          s.log.info("successfully uploaded: " + file + " to " + path)
      }
      res
    }

    val basePath =  if (sbtPlugin)
      "/"+organization + "/" + name + "/scala_" + fixedScalaVersion + "/sbt_" + fixedSbtVersion + "/" + version + "/"
    else
      "/"+organization + "/" + name + "_" + fixedScalaVersion + "/" + version + "/"
      
    val fileMap = Seq(
      //basePath + "docs" -> artifacts("doc").map{ case (a, file) => file -> a },
      basePath + "srcs" -> artifacts("src").map{ case (a, file) => file -> a },
      basePath + "jars" -> artifacts("jar").map{ case (a, file) => file -> a }
    )

    val file = ivyFile
    val path = basePath + "ivys/ivy.xml"
    val res = upload(file, path)
    
    val results =  fileMap.flatMap { case (path, files) =>
        files.map { case (file, a) =>
          path -> upload(file, path + "/" + (if (sbtPlugin)  (name + a.classifier.map("-" + _).getOrElse("") + ".jar") else (name + "_" + fixedScalaVersion + a.classifier.map("-" + _).getOrElse("") + ".jar")))
        }
    }

    results.filter{ case (_, res) => res.isRight }.map{ case (path, _) => path }
  }


}
