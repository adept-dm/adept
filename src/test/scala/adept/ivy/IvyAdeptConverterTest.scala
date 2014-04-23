package adept.ivy

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.ext.Version
import adept.repository.models.VariantHash
import adept.repository.models.RepositoryName
import adept.repository.models.ResolutionResult
import adept.repository.models.Commit
import adept.repository.models.VariantHash
import adept.repository.metadata.ResolutionResultsMetadata
import java.io.File
import adept.repository.GitRepository
import adept.repository.GitLoader
import adept.repository.metadata.VariantMetadata
import adept.utils.Hasher
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.lockfile.Lockfile
import adept.artifact.models.Artifact
import adept.artifact.models.ArtifactHash
import adept.resolution.models.Variant
import adept.resolution.models.Constraint
import adept.resolution.models.Id
import adept.resolution.models.Requirement
import adept.ext.VersionScanner
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.core.module.descriptor.DefaultModuleDescriptor
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.module.descriptor.ExcludeRule
import org.apache.ivy.core.module.descriptor.DefaultExcludeRule
import org.apache.ivy.core.module.descriptor.DefaultArtifact
import org.apache.ivy.core.module.id.ArtifactId
import org.apache.ivy.core.module.id.ModuleId
import org.apache.ivy.plugins.matcher.ExactPatternMatcher
import org.apache.ivy.core.module.descriptor.DefaultDependencyDescriptor
import adept.resolution.resolver.models.ResolvedResult
import adept.resolution.models.Attribute
import adept.test.TestDetails
import adept.ext.VersionRank
import adept.ivy.scalaspecific.ScalaBinaryVersionConverter
import adept.repository.metadata.RankingMetadata
import adept.repository.RankLogic
import adept.ext.MetadataUpdate
import adept.resolution.resolver.models.UnderconstrainedResult
import adept.test.IvyTestUtils

class IvyAdeptConverterTest extends FunSuite with Matchers {
  import adept.test.ResolverUtils._
  import adept.test.CacheUtils._
  import adept.test.FileUtils.usingTmpDir
  import adept.test.BenchmarkUtils._ //convert to benchmark hashes
  import adept.test.OutputUtils._
  import adept.test.EitherUtils._
  import adept.test.ArtifactUtils._

  import IvyConstants._
  import IvyUtils.withConfiguration
  import adept.ext.AttributeDefaults._

  val akkaTransitiveIds: Set[Id] = Set(
    "com.typesafe.akka/akka-actor_2.10",
    "org.scala-lang/scala-library",
    "com.typesafe/config")
  val akkaTransitiveIdsExpectedIds =
    (for {
      id <- akkaTransitiveIds
      confName <- Set("runtime", "provided", "javadoc", "system", "default", "sources", "compile") //optional is excluded
    } yield {
      withConfiguration(id, confName)
    }) ++ akkaTransitiveIds

  def defaultEmptyModule = {
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
    ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))
    ivyModule
  }

  val transitive = true
  val changing = true
  val force = true

  def getAkka205TestIvyModule = {
    val ivyModule = defaultEmptyModule
    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actor", "2.0.5"), force, changing, transitive)
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    ivyModule
  }

  def getScala292TestIvyModule = {
    val ivyModule = defaultEmptyModule

    val dep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("org.scala-lang", "scala-library", "2.9.2"), force, changing, transitive)
    dep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(dep)
    ivyModule
  }

  def getScala2102TestIvyModule = {
    val ivyModule = defaultEmptyModule

    val dep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("org.scala-lang", "scala-library", "2.10.2"), force, changing, transitive)
    dep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(dep)
    ivyModule
  }

  def getAkka210TestIvyModule = {
    val ivyModule = defaultEmptyModule

    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actor_2.10", "2.1.0"), force, changing, transitive)
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    ivyModule
  }

  def getAkka221TestIvyModule = {
    val ivyModule = defaultEmptyModule

    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-actor_2.10", "2.2.1"), force, changing, transitive)
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    ivyModule
  }

  private def verifyLocation(artifacts: Set[Artifact], ending: String) = {
    artifacts.foreach { artifact =>
      artifact.locations.toSeq match {
        case elem +: Nil => assert(elem.endsWith(ending), "Location: " + elem + " does not end with: '" + ending + "'")
        case _ => assert(false, "Did not find expected location ending with: '" + ending + "'. Found " + artifact.locations)
      }
    }
  }

  test("IvyImport basics: import of akka should yield correct results") {
    implicit val testDetails = TestDetails("Basic import akka 2.1.0")
    val ivy = IvyTestUtils.ivy
    ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))

    val ivyConverter = new IvyAdeptConverter(ivy)
    val ivyModule = getAkka210TestIvyModule
    val (results, _) = benchmark(IvyImport, ivyModule) {
      ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
    }
    val byIds = results.groupBy(_.variant.id)
    byIds.keySet.intersect(akkaTransitiveIdsExpectedIds) should have size (akkaTransitiveIdsExpectedIds.size)

    results.foreach {
      case result =>
        if (result.variant.id == withConfiguration("com.typesafe.akka/akka-actor_2.10", "master")) {
          verifyLocation(result.artifacts, "akka-actor_2.10-2.1.0.jar")
        }
        if (result.variant.id == withConfiguration("com.typesafe/config", "master")) {
          verifyLocation(result.artifacts, "config-1.0.0.jar")
        }
        if (result.variant.id == withConfiguration("org.scala-lang/scala-library", "master")) {
          verifyLocation(result.artifacts, "scala-library-2.10.0.jar")
        }
        //
        if (result.variant.id == withConfiguration("com.typesafe.akka/akka-actor_2.10", "compile")) {
          result.variant.requirements.map(_.id) shouldEqual Set(Id("com.typesafe.akka/akka-actor_2.10"), Id("org.scala-lang/scala-library"), withConfiguration("org.scala-lang/scala-library", "compile"), withConfiguration("org.scala-lang/scala-library", "master"), Id("com.typesafe/config"), withConfiguration("com.typesafe/config", "compile"), withConfiguration("com.typesafe/config", "master"))
          result.repository shouldEqual RepositoryName("com.typesafe.akka")
          result.versionInfo.map { case (name, _, version) => name -> version } shouldEqual Set(
            (RepositoryName("org.scala-lang"), Version("2.10.0")),
            (RepositoryName("com.typesafe"), Version("1.0.0")))
        }
    }
  }

  test("IvyImport end-to-end: import of akka should resolve correctly") {
    implicit val testDetails = TestDetails("End-to-end (akka-remote & scalatest)")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))

      val ivyConverter = new IvyAdeptConverter(ivy)
      val ivyModule = getAkka210TestIvyModule
      val (results, versionInfos) = benchmark(IvyImport, ivyModule) {
        ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
      }

      benchmark(Inserted, results) {
        IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
      }

      val configuredResolutionsResults = versionInfos.map {
        case (conf, versionInfo) =>
          conf -> {
            val (errors, results) = VersionRank.createResolutionResults(tmpDir, versionInfo)
            errors should have size(0)
            results
          }
      }
      val allConfiguredResolutionResults = //use default and compile resolution results
        configuredResolutionsResults("master") ++
          configuredResolutionsResults("compile")
      val loader = benchmark(IvyImport, allConfiguredResolutionResults) {
        new GitLoader(tmpDir, allConfiguredResolutionResults, progress, cacheManager)
      }
      val requirements = Set(
        Requirement("com.typesafe.akka/akka-actor_2.10", Set.empty, Set.empty),
        Requirement(withConfiguration("com.typesafe.akka/akka-actor_2.10", "compile"), Set.empty, Set.empty),
        Requirement(withConfiguration("com.typesafe.akka/akka-actor_2.10", "master"), Set.empty, Set.empty))
      val result = benchmark(Resolved, requirements && loader) {
        resolve(requirements, loader)
      }
      checkResolved(result, Set[Id](
        "com.typesafe/config/config/master",
        "org.scala-lang/scala-library/config/compile",
        "com.typesafe/config/config/compile",
        "com.typesafe.akka/akka-actor_2.10",
        "com.typesafe.akka/akka-actor_2.10/config/compile",
        "com.typesafe/config",
        "com.typesafe.akka/akka-actor_2.10/config/master",
        "org.scala-lang/scala-library",
        "org.scala-lang/scala-library/config/master"))
      checkAttributeVariants(result, "com.typesafe.akka/akka-actor_2.10", version -> Set("2.1.0"))
      checkAttributeVariants(result, "com.typesafe/config", version -> Set("1.0.0"))
      checkAttributeVariants(result, "org.scala-lang/scala-library", version -> Set("2.10.0"))
      //artifacts:
      checkArtifactFilename(new GitRepository(tmpDir, RepositoryName("com.typesafe.akka")), result.state.resolvedVariants("com.typesafe.akka/akka-actor_2.10/config/master"))
      checkArtifactFilename(new GitRepository(tmpDir, RepositoryName("com.typesafe")), result.state.resolvedVariants("com.typesafe/config/config/master"))
      checkArtifactFilename(new GitRepository(tmpDir, RepositoryName("org.scala-lang")), result.state.resolvedVariants("org.scala-lang/scala-library/config/master"))
      //artifact cache:
      checkArtifactCache(tmpDir, result.state.resolvedVariants("com.typesafe.akka/akka-actor_2.10/config/master"))
      checkArtifactCache(tmpDir, result.state.resolvedVariants("com.typesafe/config/config/master"))
      checkArtifactCache(tmpDir, result.state.resolvedVariants("org.scala-lang/scala-library/config/master"))
    }
  }

  def getAkkaRemoteTestIvyModule = {
    val transitive = true
    val changing = true
    val force = true
    val ivyModule = DefaultModuleDescriptor.newBasicInstance(ModuleRevisionId.newInstance("com.adepthub", "test", "1.0"), new java.util.Date(1395315115209L))
    ivyModule.addConfiguration(new IvyConfiguration("default", IvyConfiguration.Visibility.PUBLIC, "", Array("master", "runtime"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("master", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("runtime", IvyConfiguration.Visibility.PUBLIC, "", Array("compile"), true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("compile", IvyConfiguration.Visibility.PUBLIC, "", Array.empty, true, ""))
    ivyModule.addConfiguration(new IvyConfiguration("test", IvyConfiguration.Visibility.PRIVATE, "", Array("runtime"), true, ""))

    val akkaDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("com.typesafe.akka", "akka-remote_2.10", "2.2.1"), force, changing, transitive)
    akkaDep.addExcludeRule("compile", new DefaultExcludeRule(new ArtifactId(new ModuleId("com.google.protobuf", "protobuf-java"), "*", "*", "*"), ExactPatternMatcher.INSTANCE, new java.util.HashMap()))
    akkaDep.addDependencyConfiguration("compile", "default(compile)")
    ivyModule.addDependency(akkaDep)
    val scalaTestDep = new DefaultDependencyDescriptor(ivyModule,
      ModuleRevisionId.newInstance("org.scalatest", "scalatest_2.10", "1.9.1"), force, changing, transitive)
    scalaTestDep.addDependencyConfiguration("test", "default(compile)")
    ivyModule.addDependency(scalaTestDep)
    ivyModule
  }

  private val fakeIvyResults: Set[IvyImportResult] = generateFakeIvyResults("com.typesafe.akka", "akka-remote_2.10", "2.2.1", Set.empty) ++ generateFakeIvyResults("org.scalatest", "scalatest_2.10", "1.9.1", Set.empty)

  test("Ivy requirements conversion") {
    val ivyModule = getAkkaRemoteTestIvyModule
    val currentFakeIvyResults: Set[IvyImportResult] = (fakeIvyResults + IvyImportResult( //BOGUS: to make test slightly harder to pass
      variant = (Variant(
        id = Id("akka-remote_2.10/config/bogus"),
        attributes = Set(
          Attribute(ConfigurationAttribute, Set("bogus")),
          Attribute(IvyNameAttribute, Set("akka-remote_2.10")),
          Attribute(VersionAttribute, Set("2.2.1")),
          Attribute(IvyOrgAttribute, Set("com.typesafe.akka"))))),
      repository = RepositoryName("com.typesafe.akka"),
      artifacts = Set.empty, localFiles = Map.empty,
      versionInfo = Set.empty, extendsIds = Set.empty,
      excludeRules = Map.empty))

    val requirements = IvyRequirements.convertIvyAsRequirements(ivyModule, currentFakeIvyResults)

    requirements("compile") shouldEqual Set(
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/default"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/master"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/compile"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/runtime"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/test"), Set.empty, Set.empty))

    requirements("test") shouldEqual Set(
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/default"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/master"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/compile"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/runtime"), Set.empty, Set.empty),
      Requirement(Id("com.typesafe.akka/akka-remote_2.10/config/test"), Set.empty, Set.empty),
      Requirement(Id("org.scalatest/scalatest_2.10/config/default"), Set.empty, Set.empty),
      Requirement(Id("org.scalatest/scalatest_2.10/config/master"), Set.empty, Set.empty),
      Requirement(Id("org.scalatest/scalatest_2.10/config/compile"), Set.empty, Set.empty),
      Requirement(Id("org.scalatest/scalatest_2.10/config/test"), Set.empty, Set.empty),
      Requirement(Id("org.scalatest/scalatest_2.10/config/runtime"), Set.empty, Set.empty))

  }

  def generateFakeIvyResults(org: String, name: String, version: String, dependencies: Set[(RepositoryName, Id, String, String)]) = {
    val compileVersionInfo = dependencies.map {
      case (name, id, binaryVersion, version) =>
        (name, id, Version(version))
    }
    val compileRequirements = dependencies.map {
      case (name, id, binaryVersion, version) =>
        Requirement(id, Set(Constraint(BinaryVersionAttribute, Set(binaryVersion))), Set.empty)
    }
    val baseId = org + "/" + name
    val configurationHash = Hasher.hash((org + name + version).getBytes)
    val configurationRequirements = Set(Requirement(Id(baseId), Set(Constraint(ConfigurationHashAttribute, Set(configurationHash))), Set.empty))
    Set(
      IvyImportResult(
        variant = (Variant(
          id = Id(baseId),
          attributes = Set(
            Attribute(ConfigurationHashAttribute, Set(configurationHash)),
            Attribute(IvyNameAttribute, Set(name)),
            Attribute(VersionAttribute, Set(version)),
            Attribute(IvyOrgAttribute, Set(org))))),
        repository = RepositoryName(org),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, extendsIds = Set.empty,
        excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = withConfiguration(baseId, "default"),
          requirements = configurationRequirements,
          attributes = Set(
            Attribute(ConfigurationHashAttribute, Set(configurationHash)),
            Attribute(ConfigurationAttribute, Set("default")),
            Attribute(IvyNameAttribute, Set(name)),
            Attribute(VersionAttribute, Set(version)),
            Attribute(IvyOrgAttribute, Set(org))))),
        repository = RepositoryName(org),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, extendsIds = Set(baseId, withConfiguration(baseId, "runtime"), withConfiguration(baseId, "master")),
        excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = withConfiguration(baseId, "compile"),
          requirements = compileRequirements ++ configurationRequirements,
          attributes = Set(
            Attribute(ConfigurationHashAttribute, Set(configurationHash)),
            Attribute(ConfigurationAttribute, Set("compile")),
            Attribute(IvyNameAttribute, Set(name)),
            Attribute(VersionAttribute, Set(version)),
            Attribute(IvyOrgAttribute, Set(org))))),
        repository = RepositoryName(org),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = compileVersionInfo, extendsIds = Set(baseId),
        excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = withConfiguration(baseId, "runtime"),
          requirements = configurationRequirements,
          attributes = Set(
            Attribute(ConfigurationHashAttribute, Set(configurationHash)),
            Attribute(ConfigurationAttribute, Set("runtime")),
            Attribute(IvyNameAttribute, Set(name)),
            Attribute(VersionAttribute, Set(version)),
            Attribute(IvyOrgAttribute, Set(org))))),
        repository = RepositoryName(org),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, extendsIds = Set(baseId, withConfiguration(baseId, "compile")),
        excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = withConfiguration(baseId, "test"),
          requirements = configurationRequirements,
          attributes = Set(
            Attribute(ConfigurationHashAttribute, Set(configurationHash)),
            Attribute(ConfigurationAttribute, Set("master")),
            Attribute(IvyNameAttribute, Set(name)),
            Attribute(VersionAttribute, Set(version)),
            Attribute(IvyOrgAttribute, Set(org))))),
        repository = RepositoryName(org),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, extendsIds = Set(baseId, withConfiguration(baseId, "runtime")),
        excludeRules = Map.empty),
      IvyImportResult(
        variant = (Variant(
          id = withConfiguration(baseId, "master"),
          requirements = configurationRequirements,
          attributes = Set(
            Attribute(ConfigurationHashAttribute, Set(configurationHash)),
            Attribute(ConfigurationAttribute, Set("master")),
            Attribute(IvyNameAttribute, Set(name)),
            Attribute(VersionAttribute, Set(version)),
            Attribute(IvyOrgAttribute, Set(org))))),
        repository = RepositoryName(org),
        artifacts = Set.empty, localFiles = Map.empty,
        versionInfo = Set.empty, extendsIds = Set(baseId),
        excludeRules = Map.empty))
  }

  test("End-to-end: Resolution with generated Ivy results (to verify resolution engine correctness on Ivy results)") {
    implicit val testDetails = TestDetails("Resolution with generated Ivy results (to verify resolution engine correctness on Ivy results)")

    usingTmpDir { tmpDir =>
      def semver(name: String, baseId: String) = {
        val repository = new GitRepository(tmpDir, RepositoryName(name))
        Set("", "master", "runtime", "provided", "javadoc", "system", "default", "sources", "compile").foreach { conf =>
          val id = if (conf.nonEmpty) withConfiguration(baseId, conf) else Id(baseId)
          val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(id, repository, repository.getHead, includes = Set.empty, excludes = Set.empty, useVersionAsBinary = Set.empty)
          repository.add(addFiles)
          repository.rm(rmFiles)
        }
        repository.commit("Semver")
      }

      val scalaResults = generateFakeIvyResults("org.scala-lang", "scala-library", "2.10.2", Set.empty) ++
        generateFakeIvyResults("org.scala-lang", "scala-library", "2.9.2", Set.empty)
      benchmark(Inserted, scalaResults) {
        IvyImportResultInserter.insertAsResolutionResults(tmpDir, scalaResults, progress)
      }

      semver("org.scala-lang", "org.scala-lang/scala-library")

      val results1 =
        generateFakeIvyResults("com.typesafe.akka", "akka-actor", "2.0.5", Set.empty)

      val results2 =
        generateFakeIvyResults("com.typesafe.akka", "akka-actor", "2.2.1", Set(
          (RepositoryName("org.scala-lang"), withConfiguration("org.scala-lang/scala-library", "compile"), "2.10", "2.10.2"),
          (RepositoryName("org.scala-lang"), withConfiguration("org.scala-lang/scala-library", "master"), "2.10", "2.10.2")))

      benchmark(Inserted, results1) {
        IvyImportResultInserter.insertAsResolutionResults(tmpDir, results1, progress)
      }
      benchmark(Inserted, results2) {
        IvyImportResultInserter.insertAsResolutionResults(tmpDir, results2, progress)
      }

      semver("com.typesafe.akka", "com.typesafe.akka/akka-actor")

      val akkaRepo = new GitRepository(tmpDir, RepositoryName("com.typesafe.akka"))
      akkaRepo.add(MetadataUpdate.updateRepositoryResolutionResults(akkaRepo))
      akkaRepo.commit("Self update")

      val scalaRepo = new GitRepository(tmpDir, RepositoryName("org.scala-lang"))
      scalaRepo.add(MetadataUpdate.updateRepositoryResolutionResults(scalaRepo))
      scalaRepo.commit("Self update")

      withClue("Should resolve because scala lib is set to 2.9 and there is a akka-actor version that does not care about scala vesions so 2.9 is fine") {
        val requirementsWithResults = Set(
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "compile"), Set(BinaryVersionAttribute -> Set("2.9")), Set.empty),
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "master"), Set(BinaryVersionAttribute -> Set("2.9")), Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "compile"), Set.empty, Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "master"), Set.empty, Set.empty))

        val requirements = requirementsWithResults.map(_._2)

        val resultsWithLocations = GitLoader.getLatestResolutionResults(tmpDir, requirementsWithResults, progress, cacheManager)
        val results = resultsWithLocations.map(_._1)
        val loader = benchmark(IvyImport, results) {
          new GitLoader(tmpDir, results, progress, cacheManager)
        }
        val result = benchmark(Resolved, requirements && loader) {
          resolve(requirements, loader)
        }
        checkResolved(result, Set[Id](
          "com.typesafe.akka/akka-actor",
          "com.typesafe.akka/akka-actor/config/compile",
          "com.typesafe.akka/akka-actor/config/master",
          "org.scala-lang/scala-library",
          "org.scala-lang/scala-library/config/compile",
          "org.scala-lang/scala-library/config/master"))
        checkAttributeVariants(result, "com.typesafe.akka/akka-actor", "version" -> Set("2.0.5"))
        checkAttributeVariants(result, "org.scala-lang/scala-library", "version" -> Set("2.9.2"))
      }

      withClue("Cannot resolve because both akka-actor 2.0.5 and 2.2.1 accepts 2.10 as a result") {
        val requirementsWithResults = Set(
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "compile"), Set(BinaryVersionAttribute -> Set("2.10")), Set.empty),
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "master"), Set(BinaryVersionAttribute -> Set("2.10")), Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "compile"), Set.empty, Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "master"), Set.empty, Set.empty))

        val requirements = requirementsWithResults.map(_._2)

        val resultsWithLocations = GitLoader.getLatestResolutionResults(tmpDir, requirementsWithResults, progress, cacheManager)
        val results = resultsWithLocations.map(_._1)
        val loader = benchmark(IvyImport, results) {
          new GitLoader(tmpDir, results, progress, cacheManager)
        }
        val result = benchmark(Resolved, requirements && loader) {
          resolve(requirements, loader)
        }
        result.asInstanceOf[UnderconstrainedResult].optimalStates should have size (2)
        checkUnresolved(result, Set[Id](
          "com.typesafe.akka/akka-actor/config/compile",
          "com.typesafe.akka/akka-actor/config/master"))
        checkResolved(result, Set[Id](
          "org.scala-lang/scala-library",
          "org.scala-lang/scala-library/config/compile",
          "org.scala-lang/scala-library/config/master"))
      }

      withClue("Resolves fine because akka-actor 2.2 is constrained to scala library 2.10") {
        val requirementsWithResults = Set(
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "compile"), Set(BinaryVersionAttribute -> Set("2.2")), Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "master"), Set(BinaryVersionAttribute -> Set("2.2")), Set.empty))

        val requirements = requirementsWithResults.map(_._2)

        val resultsWithLocations = GitLoader.getLatestResolutionResults(tmpDir, requirementsWithResults, progress, cacheManager)
        val results = resultsWithLocations.map(_._1)
        val loader = benchmark(IvyImport, results) {
          new GitLoader(tmpDir, results, progress, cacheManager)
        }
        val result = benchmark(Resolved, requirements && loader) {
          resolve(requirements, loader)
        }
        checkResolved(result, Set[Id](
          "com.typesafe.akka/akka-actor",
          "com.typesafe.akka/akka-actor/config/compile",
          "com.typesafe.akka/akka-actor/config/master",
          "org.scala-lang/scala-library",
          "org.scala-lang/scala-library/config/compile",
          "org.scala-lang/scala-library/config/master"))
        checkAttributeVariants(result, "com.typesafe.akka/akka-actor", "version" -> Set("2.2.1"))
        checkAttributeVariants(result, "org.scala-lang/scala-library", "version" -> Set("2.10.2"))
      }

      withClue("Resolves fine because akka-actor 2.2 is already! constrained to scala library 2.10") {
        val requirementsWithResults = Set(
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "compile"), Set(BinaryVersionAttribute -> Set("2.10")), Set.empty),
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "master"), Set(BinaryVersionAttribute -> Set("2.10")), Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "compile"), Set(BinaryVersionAttribute -> Set("2.2")), Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "master"), Set(BinaryVersionAttribute -> Set("2.2")), Set.empty))

        val requirements = requirementsWithResults.map(_._2)

        val resultsWithLocations = GitLoader.getLatestResolutionResults(tmpDir, requirementsWithResults, progress, cacheManager)
        val results = resultsWithLocations.map(_._1)
        val loader = benchmark(IvyImport, results) {
          new GitLoader(tmpDir, results, progress, cacheManager)
        }
        val result = benchmark(Resolved, requirements && loader) {
          resolve(requirements, loader)
        }
        checkResolved(result, Set[Id](
          "com.typesafe.akka/akka-actor",
          "com.typesafe.akka/akka-actor/config/compile",
          "com.typesafe.akka/akka-actor/config/master",
          "org.scala-lang/scala-library",
          "org.scala-lang/scala-library/config/compile",
          "org.scala-lang/scala-library/config/master"))
        checkAttributeVariants(result, "com.typesafe.akka/akka-actor", "version" -> Set("2.2.1"))
        checkAttributeVariants(result, "org.scala-lang/scala-library", "version" -> Set("2.10.2"))
      }
    }
  }

  test("Conversions of Ivy results yields what we expect") {
    implicit val testDetails = TestDetails("Conversions of Ivy results yields what we expect")
    usingTmpDir { tmpDir =>
      val ivy = IvyTestUtils.ivy
      ivy.configure(new File("src/test/resources/typesafe-ivy-settings.xml"))

      def insertIvyModule(ivyModule: ModuleDescriptor) = {
        val ivyConverter = new IvyAdeptConverter(ivy)

        val (results, _) = benchmark(IvyImport, ivyModule) {
          ivyConverter.loadAsIvyImportResults(ivyModule, progress).failOnLeft
        }
        val insertedResults = benchmark(Inserted, results) {
          IvyImportResultInserter.insertAsResolutionResults(tmpDir, results, progress)
        }
        insertedResults.map { r =>
          r.repository -> r.id
        }
      }
      def semverScalaAkka(idsAndRepos: Set[(RepositoryName, Id)]) = {
        val semverResults = idsAndRepos
          .filter { case (_, id) => id.value.contains("org.scala-lang") || id.value.contains("com.typesafe.akka") }

        semverResults.foreach {
          case (name, id) =>
            val repository = new GitRepository(tmpDir, name)
            val commit = repository.getHead
            val (addFiles, rmFiles) = VersionRank.useSemanticVersionRanking(id, repository, commit, includes = Set(".*".r), excludes = Set.empty, useVersionAsBinary = Set.empty)
            repository.add(addFiles)
            repository.rm(rmFiles)
        }
        semverResults.map(_._1).foreach { name =>
          val repository = new GitRepository(tmpDir, name)
          repository.commit("Semver")
        }
      }
      semverScalaAkka(insertIvyModule(getScala292TestIvyModule) ++
        insertIvyModule(getScala2102TestIvyModule))

      semverScalaAkka(insertIvyModule(getAkka205TestIvyModule) ++
        insertIvyModule(getAkka221TestIvyModule))

      val requirementsWithResults = Set(
        RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "compile"), Set(BinaryVersionAttribute -> Set("2.10")), Set.empty),
        RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "master"), Set(BinaryVersionAttribute -> Set("2.10")), Set.empty),
        RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "compile"), Set(BinaryVersionAttribute -> Set("2.1")), Set.empty),
        RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "master"), Set(BinaryVersionAttribute -> Set("2.1")), Set.empty))

      val resultsWithLocations = GitLoader.getLatestResolutionResults(tmpDir, requirementsWithResults, progress, cacheManager)
      println(resultsWithLocations)

      withClue("Should resolve because scala lib is set to 2.9 and there is a akka-actor version that does not care about scala vesions so 2.9 is fine") {
        val requirementsWithResults = Set(
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "compile"), Set(BinaryVersionAttribute -> Set("2.9")), Set.empty),
          RepositoryName("org.scala-lang") -> Requirement(withConfiguration("org.scala-lang/scala-library", "master"), Set(BinaryVersionAttribute -> Set("2.9")), Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "compile"), Set.empty, Set.empty),
          RepositoryName("com.typesafe.akka") -> Requirement(withConfiguration("com.typesafe.akka/akka-actor", "master"), Set.empty, Set.empty))

        val requirements = requirementsWithResults.map(_._2)

        val resultsWithLocations = GitLoader.getLatestResolutionResults(tmpDir, requirementsWithResults, progress, cacheManager)
        val results = resultsWithLocations.map(_._1)
        val loader = benchmark(IvyImport, results) {
          new GitLoader(tmpDir, results, progress, cacheManager)
        }
        val result = benchmark(Resolved, requirements && loader) {
          resolve(requirements, loader)
        }
        println(result)
        checkResolved(result, Set[Id](
          "com.typesafe.akka/akka-actor",
          "com.typesafe.akka/akka-actor/config/compile",
          "com.typesafe.akka/akka-actor/config/master",
          "com.typesafe/config",
          "com.typesafe/config/config/compile",
          "com.typesafe/config/config/master",
          "org.scala-lang/scala-library",
          "org.scala-lang/scala-library/config/compile",
          "org.scala-lang/scala-library/config/master"))
        checkAttributeVariants(result, "com.typesafe.akka/akka-actor", "version" -> Set("2.0.5"))
        checkAttributeVariants(result, "org.scala-lang/scala-library", "version" -> Set("2.9.2"))
      }
    }
  }

}