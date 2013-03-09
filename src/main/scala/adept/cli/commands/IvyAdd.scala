package adept.cli.commands

import adept.ivy.IvyHelpers
import adept.core.Parsers
import adept.core.db

object IvyAddCommand extends Command {
  override val command = "ivy-add"
  override def description = """add an ivy dependency"""
  override def help = s"""
    |usage: adept $command --conf=<conf> --settings=<ivy settings> <repo> <coordinates> 
    """.stripMargin

  def getOption(opt: String, args: List[String]) = { //TODO: replace with some option library
    val Expr = (opt+"=(.*)").r
    args.find(_.startsWith(opt+"=")).map{ 
      case Expr(value) => value 
    }
  }  
  
  override def execute(args: List[String]): Either[String, String] = {
    /*
    val confArg = getOption("--conf", args)
    val settingsArg = getOption("--settings", args)
    val optsArgs = args.filter{ a =>  //TODO: This is present-fredrik writing to future-fredrik: I am sorry!
      confArg.map(c => ("--conf="+c) == a).getOrElse(false) ||
      settingsArg.map(c => ("--settings="+c) == a).getOrElse(false)
    }
    val noOptsArgs = args.diff(optsArgs)
    val repoArg = noOptsArgs.drop(0).take(1).headOption
    val coordsArg = noOptsArgs.drop(1).take(1).headOption
    val conf = confArg.getOrElse(adept.ivy.Configuration.defaultIvyConf)
    val settings = IvyHelpers.load(settingsArg)
    
    (for {
      repoName <- repoArg.toRight("please provide a repository name").right
      coordsString <- coordsArg.toRight("please provide coordinates (format: <org>:<name>:<version>)").right
      coords <- Parsers.coords(coordsString).right
      ivy <- settings.right
    } yield {
      IvyHelpers.add(repoName, coords, ivy, conf)(db.database).right.map(_.mkString("\n"))
    }).joinRight
    */
    Left("TODO")
  }
}