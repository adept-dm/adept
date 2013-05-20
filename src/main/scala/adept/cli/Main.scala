package adept.cli

import adept.cli.commands.Commands

object Main extends App {
  CliHelpers.commandParser(args.toList, Commands.all)
}
