package adept.cli

import adept.cli.commands.Commands

object Main extends App {
  CliHelpers.commandParser(args.toList, Commands.all) match {
    case Right(x) => {
      x map { println(_) }
      System.exit(0)
    }
    case Left(x) => {
      println("Error: %s" format(x))
      System.exit(1)
    }
  }
}
