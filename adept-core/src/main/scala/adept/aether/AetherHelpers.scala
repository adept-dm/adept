package adept.aether

import adept.core.models._
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.connector.wagon.WagonRepositoryConnectorFactory
import org.eclipse.aether.connector.wagon.WagonProvider
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.repository.LocalRepository
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.repository.RemoteRepository
import org.eclipse.aether.resolution.ArtifactRequest
import org.eclipse.aether.util.filter.DependencyFilterUtils
import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.collection.CollectRequest
import org.eclipse.aether.resolution.DependencyRequest
import org.eclipse.aether.resolution.ArtifactDescriptorRequest
import org.eclipse.aether.artifact.{ Artifact => AetherArtifact }
import org.eclipse.aether.graph.DependencyVisitor
import org.eclipse.aether.graph.DependencyNode


//read more?? http://stackoverflow.com/questions/10536221/fetching-maven-artifacts-programmatically
object AetherHelper extends App {

  asAdeptNode(Coordinates("play", "play_2.10", "2.1.1"), List(new RemoteRepository.Builder("typesafe", "default", "http://repo.typesafe.com/typesafe/releases/").build(),
    new RemoteRepository.Builder("central", "default", "http://repo1.maven.org/maven2/").build(),
    new RemoteRepository.Builder("maven.java.net", "default", "https://maven.java.net/content/repositories/releases").build()))

  private def defaultSession(system: RepositorySystem) = {
    val session = MavenRepositorySystemUtils.newSession();

    val localRepo = new LocalRepository("target/local-repo");
    session.setLocalRepositoryManager(system.newLocalRepositoryManager(session, localRepo));
    session.setTransferListener(new ConsoleTransferListener());
    //session.setRepositoryListener( new ConsoleRepositoryListener() );
    session
  }

  def scopeAsConfig(scope: String): String = {
    scope match {
      case "test" => "test->runtime(*),master(*)"
      case "compile" => "compile->compile(*),master(*);runtime->runtime(*)"
      case "runtime" => "runtime->compile(*),runtime(*),master(*)"
      case scope => Configuration.defaultConfigurationMapping(scope)
    }
  }

  def asAdeptNode(coords: Coordinates, repos: List[RemoteRepository]): Node = {
    import collection.JavaConverters.{ synchronized => _, _ }
    val system = ManualRepositorySystemFactory.newRepositorySystem()
    val session = defaultSession(system)

    val artifact = new DefaultArtifact(coords.org + ":" + coords.name + ":" + coords.version)
    asAdeptModule(artifact)

    def asAdeptModule(artifact: AetherArtifact) = synchronized { //Aether is not thread-safe

      val artifactRequest = new ArtifactRequest()
      artifactRequest.setArtifact(artifact)
      repos.foreach(artifactRequest.addRepository)
      val artifactDescriptorRequest = new ArtifactDescriptorRequest(artifact,
        repos.asJava, null)
      val artifactDeps = system.readArtifactDescriptor(session, artifactDescriptorRequest).getDependencies()

      val collectRequest = new CollectRequest()
      collectRequest.setDependencies(artifactDeps);
      repos.foreach(collectRequest.addRepository)
      val collectResult = system.collectDependencies(session, collectRequest)

      collectResult.getRoot().accept(new ConsoleDependencyGraphDumper)

    }
    null
  }

}