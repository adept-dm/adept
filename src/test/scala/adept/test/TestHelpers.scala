package adept.test

import adept.core.models._
import adept.core.resolution._
import org.scalatest.matchers.MustMatchers
import adept.core.models.State
import adept.ext.DefinedVariants
import java.io.File
import net.sf.ehcache.CacheManager

object TestHelpers extends MustMatchers {

  def timer(f: => Unit): Long = {
    val time = System.currentTimeMillis
    f
    System.currentTimeMillis - time
  }

  def REMOVEMEusingTempDir(f: File => Unit) = {
    val rootDir = "test-tmp"

    val tmpDir = new File(rootDir, "tmp-dir-" + System.currentTimeMillis)
    try {
      if (tmpDir.mkdirs()) {
        f(tmpDir)
      } else throw new Exception("Could not create tmp dir: " + tmpDir)
    } finally {
      //import scala.reflect.io.Directory
      //if (tmpDir.isDirectory) (new Directory(tmpDir)).deleteRecursively()
    }
  }

  def usingTempDir(f: File => Unit) = {
    val rootDir = "test-tmp"

    val tmpDir = new File(rootDir, "tmp-dir-" + System.currentTimeMillis)
    try {
      if (tmpDir.mkdirs()) {
        f(tmpDir)
      } else throw new Exception("Could not create tmp dir: " + tmpDir)
    } finally {
      import scala.reflect.io.Directory
      if (tmpDir.isDirectory) (new Directory(tmpDir)).deleteRecursively()
    }
  }

  def usingCacheManager(f: CacheManager => Unit) = {
    val cacheManager = CacheManager.create()
    try {
      f(cacheManager)
    } finally {
      cacheManager.shutdown()
    }
  }

  def load(testData: (Set[Dependency], Seq[Variant])): ResolveResult = {
    val (dependencies, all) = testData

    val resolver = new Resolver(new DefinedVariants(all))
    resolver.resolve(dependencies)
  }

  def resolved(result: ResolveResult): State = {
    assert(result.state.isResolved, "could not find resolved state:\n" + result)
    result.state
  }

  def unresolved(result: ResolveResult): State = {
    assert(!result.state.isResolved, "could not find unresolved state:\n" + result)
    result.state
  }

  def checkUnresolved(state: State, ids: Set[String]) = {
    val underconstrained = state.underconstrained
    val overconstrained = state.overconstrained
    (overconstrained ++ underconstrained).map(_.value) must equal(ids)
  }

  def checkResolved(state: State, ids: Set[String]) = {
    (state.resolved ++ state.implicitVariants.keys).map(_.value) must equal(ids)
  }

  def checkConstraints(state: State, attr: (String, (String, Set[String]))) = {
    val (id, (attrName, attrValues)) = attr
    import org.scalatest.OptionValues._
    state.constraints.get(new Id(id)).value must equal(Constraint(attrName, attrValues))
  }

  def checkVariants(state: State, attr: (String, (String, Set[String]))) = {
    val (id, (attrName, attrValues)) = attr
    import org.scalatest.OptionValues._
    val variant = (state.resolvedVariants ++ state.implicitVariants).get(new Id(id)).value
    variant.id must equal(new Id(id))
    (id -> variant.attributes) must equal(id -> Set(Attribute(attrName, attrValues)))
  }
}

object VariantsLoaderEngineTester extends VariantsLoaderLogic with MustMatchers {
  def testMatches(attributes: Set[Attribute], constraints: Set[Constraint], expectMatchValue: Boolean) = {
    assert(matches(attributes, constraints) == expectMatchValue, "expected attributes: " + attributes + " and constraints: " + constraints + " to " + (if (expectMatchValue) "" else "NOT") + " match")
  }
}