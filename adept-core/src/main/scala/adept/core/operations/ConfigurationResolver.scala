package adept.core.operations

case class ConfigParseException(msg: String) extends Exception("could not parse configuratin: " + msg)
import adept.core.models._
import adept.utils.Logging
import adept.core.Adept
import adept.utils.EitherUtils
import collection.{Set => _, _}

private[core] object ConfigurationResolver extends Logging {
  val unsupportedStrings = Set("%", "!", "[", "]", "@", "#")
  
  val ConfSep = ";"
  val Comma = ","
    
  def extendedConfs(rootConfs: Set[Configuration], conf: Configuration): Set[Configuration]= {
    val (doesExtend, doesNotExtend) = rootConfs.partition{ rootConf =>
      conf.extendsFrom.contains(rootConf.name)
    }
    doesExtend ++ doesExtend.flatMap{ extendedConf =>
      extendedConfs(doesNotExtend, extendedConf)
    }
  }
  
  private def splitDependencyConfExpr(confExpr: String) = {
    val parts = confExpr.split("->")
    if (parts.size < 2) throw new ConfigParseException("'" + confExpr + "' was expected to contain a least one '->'") 
    if (parts.size > 2) throw new ConfigParseException("'" + confExpr + "' has more than 2 parts seperated by '->'") 
    (parts.head.trim(), parts.last.trim())
  }
  
  private def findFallback(confExpr: String): (String, Option[String]) = {
    val FallbackExpr = """(.*?)\((.*?)\)$""".r
    confExpr.trim match {
      case FallbackExpr(rest, fallbackExpr) => rest.trim() -> Some(fallbackExpr.trim())
      case _ => confExpr.trim() -> None
    }
  }
  
  private def matchConf(name: String, expr: String) = {
    expr match {
      case "*" => true
      case `name` => true
      case _ => false
    }
  }
  
  private def findNames(confs: Set[Configuration], expr: String): Set[Configuration] = {
    val allConfs = expr.split(Comma).map(_.trim)
    
    confs.filter{ c =>
      allConfs.find(matchConf(c.name, _)).isDefined
    }
  }
 
  private def findMatchingRight(confs: Set[Configuration], expr: String): mutable.Set[Configuration] = {
    val allConfsFallback = expr.split(Comma).map(findFallback)
    val visibleConfs = confs.filter(_.visibility == Visibility.Public)
    
    def hasConf(conf: Configuration, expr: String, debugMsg: String) = {
      val foundConf = matchConf(conf.name, expr)
      if (foundConf) {
        logger.debug(debugMsg)
      }
      foundConf
    }
    
    allConfsFallback.flatMap{ case (confExpr, fallback) =>
      val foundConfs = visibleConfs.filter{ conf => 
        hasConf(conf, confExpr, "in: '" + conf.name + "' found conf for '" + confExpr + "' among " + confs.map(_.name).mkString(","))
      }
      if (foundConfs.isEmpty) {
        fallback.map{ fallbackConfExpr =>
          logger.debug("'" + confExpr + "' did not match any confs. searching for fallback conf '" + fallbackConfExpr + "' in '" + expr + "' among " + confs.map(_.name).mkString(","))
          visibleConfs.filter{ conf => 
            hasConf(conf, fallbackConfExpr, "in: '" + conf.name + "' found fallback conf for '" + confExpr + "' among " + confs.map(_.name).mkString(","))
          }
        }.getOrElse{
          Set.empty
        }
      } else {
        foundConfs
      }
    }(breakOut) //breakOut transform the collection to the one specified by the Type parameter, without having to reiterate over it
  }
  
  private def checkExpr(expr: String) = {
    unsupportedStrings.foreach{ s =>
      if (expr.contains(s)) throw ConfigParseException("'" + expr + "' contains an unsupported expression: " + s)
    }
  }
  
  def resolve(leftConfs: Set[Configuration], leftExpr: String): Either[String, mutable.Set[Configuration]] = {
    checkExpr(leftExpr)
    val foundConfs = mutable.Set() ++ findNames(leftConfs, leftExpr)
    if (foundConfs.nonEmpty) Right(foundConfs ++ foundConfs.flatMap(c => extendedConfs(leftConfs, c)))
    else  Left("expression '" +leftExpr+ "' does not match confs: " + leftConfs.map(_.name).mkString(","))
  }
  
  
  /**
   * Based on the matching left configurations (root) that matches the left part of exprs (e.g. in default->compile,master(*);test->compile you have default, test)
   * resolve finds the the right configurations (in the module) that matches the right part of exprs 
   * 
   * The supported syntax:
   * - '*' means all configurations
   * - ';' separates multiple expressions
   * - ',' separates multiple configurations
   * - 'A->B' means that configuration(s) matched by A in the left set of confs is mapped to B in the right set of confs
   * - using a fallback configuration, e.g.'A->B(C)'. It means that if B is found, the configuration found by C will be used. C can be either a name or a wildcard, '*'
   * 
   * See unsupportedStrings to learn about what is not supported
   * 
   */
  def resolve(leftConfs: Set[Configuration], exprs: String, rightConfs: Set[Configuration]): Either[Set[String], mutable.Set[Configuration]] ={
    checkExpr(exprs)
    def bind(expr: String) = {
      val (leftExpr, rightExpr) = splitDependencyConfExpr(expr)
      
      (for {
        matchingLeft <- resolve(leftConfs, leftExpr).right
      } yield {
        matchingLeft.foreach{ c => 
          c.deprecated.foreach(msg => logger.warn(c.name + " is deprecated " + msg))
        }
        val matchingRight = findMatchingRight(rightConfs, rightExpr)
        if (matchingRight.nonEmpty) {
          Right(matchingRight)
        } else  {
          Left("no public resolved confs for: " +rightExpr  + " in " + rightConfs.filter(_.visibility != Visibility.Private).map(_.name).mkString(",") + ". private confs are: " + rightConfs.filter(_.visibility == Visibility.Private).map(_.name).mkString(","))
        }
      }).joinRight
    }
    val binded: mutable.Set[Either[String, mutable.Set[Configuration]]]= exprs.split(ConfSep).map(bind)(breakOut)
    val matching  = binded.collect{
      case Right(matched) => matched 
    }.flatten
    
    if (matching.isEmpty) {
      Left(binded.collect{
        case Left(msg) => msg 
      }(breakOut)) //breakOut transform the collection to the one specified by the Type parameter, without having to reiterate over it
    } else {
      Right(matching)
    }
  }
  
}