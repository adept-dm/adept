package adept.sbt

import sbt.settingKey
import sbt.Command

object AdeptKeys {
  val adeptTimeout = settingKey[Int]("Timeout for downloads in minutes for adept")
  val adeptCommand = {
    import sbt.complete.DefaultParsers._

    lazy val change = Space ~> (reset | setColor)
    lazy val reset = token("reset" ^^^ "\033[0m")
    lazy val color = token(Space ~> ("blue" ^^^ "4" | "green" ^^^ "2"))
    lazy val select = token("fg" ^^^ "3" | "bg" ^^^ "4")
    lazy val setColor = (select ~ color) map { case (g, c) => "\033[" + g + c + "m" }

    Command("color")(_ => change) { (state, ansicode) =>
      print(ansicode)
      state
    }
  }
}