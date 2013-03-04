package adept.ivy
import org.scalatest._
import adept.core._
import org.apache.ivy.core.module.id.ModuleRevisionId

class IvySpec extends FreshDBEachRun with ShouldMatchers {
  import TestData._
  
  describe("Ivy") {
    it("should add the module correctly") {
      val ivy = IvyHelpers.load().right.get
      val coords = Coordinates("commons-cli", "commons-cli", "1.0")
      val expectedScope = "master"
      val expectedMeta = Metadata(Map("scope"->expectedScope))
      val expectedModule = Module(coords, expectedMeta, Hash("TODO"), Set.empty, Hash("d70644c4fc5d2a171a70b4d65f5706173de64a5b")) //TODO: artifact
      
      val res = IvyHelpers.add(repoName, coords, ivy, Configuration.defaultIvyConf)
      res should be(Right(Seq(expectedModule)))
      
      val all = Adept.describe(coords, expectedMeta)

      all.isRight should be(true)
      all.right.get._1 should be(expectedModule)
      val deps = all.right.get._2
      deps.mkString(",") should be(s"commons-logging:commons-logging:1.0[scope=$expectedScope]@7b6b102882d308ade8f5a61aea124a093d089b94,junit:junit:3.7[scope=$expectedScope]@4c10115ce4a66064d2b4e57ae58c126020c312cd,commons-lang:commons-lang:1.0[scope=$expectedScope]@eeb27b2091484a9126bdecf220d3a27a6e839508")
    }
  }
}