package adept.core

import org.scalatest._

class ParserSpec extends FunSpec with ShouldMatchers {
  def checkLeftContains(res: Either[String, _], includesWords: String*) = {
    res.isLeft should be === true
    includesWords.foreach { w =>
      res.left.get.toLowerCase should include(w)
    }
  }
  
  describe("Parsers") {
    it("should parse correct coordinates") {
      val testCoords = Coordinates("play", "play", "2.142") 
      Parsers.coords(testCoords.toString) should equal (Right(testCoords))
      Parsers.coords("play:play:2.142") should equal (Right(testCoords))
      
      checkLeftContains(Parsers.coords("play:play2.2"), "not", "parse", "coordinates")
    }
    
    it("should parse metadata correctly") {
      val Right(m) = Parsers.metadata("[test=foo,fas=adfas]")
      Parsers.metadata(m.toString) should equal(Right(m))
    }
    
    it("should parse correct coordinates with metadata") {
      val testCoords = Coordinates("play", "play", "2.142") 
      val singleTuple = "foo" -> "123"
      val testCoodsMetadata = testCoords -> Metadata(data = Map(singleTuple , "test" -> "2341"))
      
      Parsers.coordsMetadata("play:play:2.142[foo=123]") should equal (Right(testCoords -> Metadata(data = Map(singleTuple))))
      Parsers.coordsMetadata("play:play:2.142[]") should equal (Right(testCoords -> Metadata(data = Map.empty)))
      
      //check for correct whitespace tolerance:
      Parsers.coordsMetadata("play:play:2.142[foo=123, test=2341]") should equal(Right(testCoodsMetadata))
      Parsers.coordsMetadata(" play:play:2.142[foo=123,test=2341] ") should equal(Right(testCoodsMetadata))
      checkLeftContains(Parsers.coordsMetadata("play:play: 2.142[foo=123,test=21321]"), "not", "parse", "whitespace", "coordinates")
      
      //errors:
      checkLeftContains(Parsers.coordsMetadata("play:play2.2"), "not", "parse", "coordinates")
      checkLeftContains(Parsers.coordsMetadata("play:plays2.2[fd=1231]"), "not", "parse", "coordinates")
      checkLeftContains(Parsers.coordsMetadata("play:plays:2.2[fd]"), "not", "parse", "metadata")
      checkLeftContains(Parsers.coordsMetadata("play:plays:2.2[fd=,12]"), "not", "parse", "metadata")
      checkLeftContains(Parsers.coordsMetadata("play:plays:2.2[fd=12,1213]"), "not", "parse", "metadata")
    }
  }
}