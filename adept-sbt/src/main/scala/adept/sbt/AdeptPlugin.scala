package adept.sbt

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._
import sbt.complete._

object AdeptPlugin extends Plugin {
  import AdeptKeys._

  def adeptSettings = Seq(
    adeptTimeout := 60, //minutes,
    sbt.Keys.commands += {
      val reset = token("reset" ^^^ "\033[0m")
      val color = token(Space ~> ("blue" ^^^ "4" | "green" ^^^ "2"))
      val select = token("fg" ^^^ 1 | "bg" ^^^ 4)
      val setColor = (select ~ color) map { case (g, c) => (g, c)}
      val change = Space ~> (reset | setColor)

      Command("color")(_ => change) { (state, ansicode) =>
        print(ansicode)
        state
      }
    })
}