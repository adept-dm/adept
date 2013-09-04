package adept.examples

import adept.core.models.{ Artifact, Coordinates, Configuration, Dependency, Hash, Module, Override, UniqueId, Universe, Visibility }
import adept.core.Adept
import java.io.File


object BasicResolve extends App {

  //our dependencies
  val dependencies = Set(
    Dependency(Coordinates("org.eclipse.jgit","org.eclipse.jgit","1.3.0.201202151440-r"), None, "compile->compile(*),master(*);runtime->runtime(*)"), //uses the most common configuration mapping, this is the default and usually provided if nothing else is specified
    Dependency(Coordinates("junit", "junit", "4.10"), Some(UniqueId("junit:junit:4.10-3c21c63")), "test") //this will be mapped using the configurationMapping below
  )
  
  val configurations = Set( //a set the most common configurations:
    Configuration("compile", description = None, extendsFrom = Set.empty, visibility = Visibility.Public, None),
    Configuration("master", description = None, extendsFrom = Set.empty, visibility = Visibility.Public, None),
    Configuration("runtime", description = None, extendsFrom = Set("compile"), visibility = Visibility.Public, None),
    Configuration("default", description = None, extendsFrom = Set("runtime", "master"), visibility = Visibility.Public, None),
    Configuration("test", description = None, extendsFrom = Set("runtime"), visibility = Visibility.Private, None)
  )


  def clean(f: File) = { //used to clean up this example
    def deleteRecursively(f: File): Unit = {
      if (f.isDirectory) f.listFiles match { case null => case xs => xs foreach deleteRecursively }
      f.delete()
    }
    println("removing " + f)
    deleteRecursively(f)
  }

  val exampleDir = new File("adept-example-files") //typically ~/.adept
  clean(exampleDir)

  println("cloning to " + exampleDir + "...")
  //cloning new dir: see Adept.init, Adept.open, pull() on Adept class for other examples
  val adeptCentral = Adept.clone(exampleDir, name = "central", uri = "https://github.com/freekh/adept-central.git").right.get //throws exception if isLeft
  
  def fetchArtifacts() = {
    //default configuration mapping:
    val configurationMapping: String => String = Configuration.defaultConfigurationMapping(_, "*->default(compile)") //basic mapping. example: maps 'test' to 'test->default(compile)'

    val conf = "test" //or "compile" or "test"
    val universes = Set.empty[Universe]

    val tree = Adept.resolve(
      Set(adeptCentral),
      conf,
      dependencies,
      universes,
      configurations,
      configurationMapping
    ).right.get //throws exception if isLeft

    println("dumping tree...")
    println(tree)

    import akka.util.duration._ //TODO: should be using scala time instead
    val hashLocations = tree.artifacts.map{ artifact =>
      artifact.hash -> artifact.locations
    }.toSeq

    val files = Adept.fetch(exampleDir, hashLocations, timeout = 5.minutes).right.get //throws exception if isLeft

    println("downloaded " + files.mkString(","))
  }

  def addModule() = {
    val coords = Coordinates("my.org", "name", "1.0")

    //see Artifact.fromFile to load Hash from a jar file for example
    val artifacts = Set(Artifact(Hash("uniqueId"), "jar", Set("master"), Set("http://mysite/somefile.jar")))
    val uniqueId = UniqueId.default(coords, new java.util.Date, artifacts)
    val universes = Set(Universe("scala-version", "2.9.2")) //an example of the scala-version universe
    val overrides = Set(Override("junit", "junit", "4.10", None))
    val attributes = Map("security-update" -> Seq("true")) //persisted attributes, can be anything

    val module = Module(
      coordinates = coords,
      uniqueId = uniqueId,
      universes = universes,
      artifacts = artifacts,
      configurations = configurations,
      attributes = attributes,
      dependencies = dependencies,
      overrides = overrides
    )

    adeptCentral.add(module)
  }

  fetchArtifacts()
  addModule()

}
