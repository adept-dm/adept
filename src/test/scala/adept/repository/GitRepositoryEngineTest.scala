package adept.repository

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import java.io.File
import adept.core.models._
import adept.test.TestHelpers
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.merge.MergeStrategy
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.Ref
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.PathFilter
import org.eclipse.jgit.revwalk.filter.RevFilter

class GitRepositoryEngineTest extends FunSuite with MustMatchers {
  import TestHelpers._

  test("basic variant read/write git repo engine test") {
    import EitherValues._
    import OptionValues._
    usingTempDir { tmpDir =>
      usingCacheManager { cacheManager =>
        val baseDir = tmpDir
        val repoName = "hodor"
        //TODO: I do not like these tests. consider removing them and putting everything in AdeptRepositoryManager test
        val repoHandle = AdeptRepositoryManager.init(baseDir, repoName)
        val repo = repoHandle.result.right.value.headOption.value

        val variantA1 = Variant(Id("A"), Set(ArtifactRef(Hash("123"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A1"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
        val variantA2 = Variant(Id("A"), Set(ArtifactRef(Hash("123"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A2"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
        val variantB = Variant(Id("B"), Set(ArtifactRef(Hash("456"), Set(Attribute("master", Set("compile"))), Some("filenameB"))), Set(Attribute("version", Set("B", "Bogus"))), Set(Dependency(new Id("C"), Set(Constraint("version", Set("X"))))))

        repo.writeVariant(variantA1)
        repo.writeVariant(variantA2)
        repo.writeVariant(variantB)

        val manager = new GitRepositoryEngine(tmpDir, Set(repo.commit("hooodooor")), cacheManager)

        manager.get(Id("A"), Set()) must have size (2)
        manager.get(Id("A"), Set(Constraint("version", Set("A2")))) must have size (1)
        manager.get(Id("A"), Set(Constraint("version", Set("X")))) must be('empty)

      }

    }
  }

  test("basic artifact read/write git repo engine test") {
    import EitherValues._
    import OptionValues._
    usingTempDir { tmpDir =>
      usingCacheManager { cacheManager =>

        val baseDir = tmpDir
        val repoName = "hodor"
        val repoHandle = AdeptRepositoryManager.init(baseDir, repoName)
        val repo = repoHandle.result.right.value.headOption.value

        val hashB1 = Hash("123b1")
        val hashB2 = Hash("123b2")

        val variantB = Variant(Id("B"), Set(ArtifactRef(hashB1, Set(Attribute("configuration", Set("compile", "runtime"))), Some("filenamb1.jar")), ArtifactRef(hashB2, Set(Attribute("configuration", Set("javadoc"))), None)), Set(Attribute("version", Set("1.0"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
        repo.writeVariant(variantB)
        val artifactB1 = Artifact(hashB1, 4124, Set("http://location1"))
        val artifactB2 = Artifact(hashB2, 5234, Set("http://location2"))
        repo.writeArtifactDescriptor(artifactB1)
        repo.writeArtifactDescriptor(artifactB2)

        val manager = new GitRepositoryEngine(tmpDir, Set(repo.commit("hoooodoor")), cacheManager)

        manager.getArtifacts(variantB, Set(Constraint("configuration", Set("compile", "runtime", "javadoc")))) must have size (2)
        manager.getArtifacts(variantB, Set(Constraint("configuration", Set("compile", "runtime")))) must have size (1)
        manager.getArtifacts(variantB, Set(Constraint("configuration", Set("compile")))) must have size (1)
        manager.getArtifacts(variantB, Set(Constraint("configuration", Set("runtime")))) must have size (1)
        manager.getArtifacts(variantB, Set(Constraint("configuration", Set("javadoc")))) must have size (1)
        manager.getArtifacts(variantB, Set(Constraint("configuration", Set("foo")))) must have size (0)
        manager.getArtifacts(variantB, Set(Constraint("configuration", Set()))) must have size (0)
      }
    }
  }

  //TODO: remove from here
  test("basic git bench") {
    import EitherValues._
    import OptionValues._
    usingTempDir { tmpDir =>
      usingCacheManager { cacheManager =>
        val baseDir = tmpDir
        val repoName = "foo"
        val repoHandle = AdeptRepositoryManager.init(baseDir, repoName)
        val repo = repoHandle.result.right.value.headOption.value

        println(timer(for (i <- 1 to 100) {
          val variant = Variant(Id("A"), Set(ArtifactRef(Hash("123" + i), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A" + i))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
          repo.writeVariant(variant)
        }))

        val manager = new GitRepositoryEngine(tmpDir, Set(repo.commit("foo")), cacheManager)

        //      println(timer(manager.get(Id("A"), Set()) must have size (1000)))
        //      println(timer(manager.get(Id("A"), Set(Constraint("version", Set("A2")))) must have size (1)))
        //      println(timer(manager.get(Id("A"), Set(Constraint("version", Set("X")))) must be('empty)))
        println(timer(manager.get(Id("A"), Set())))
        println(timer(manager.get(Id("A"), Set(Constraint("version", Set("A2"))))))
        println(timer(manager.get(Id("A"), Set(Constraint("version", Set("X"))))))
      }
    }
  }
  def using(git: Git, start: Ref) = new {
    def getLatest(commits: Set[RevCommit]): Option[RevCommit] = {
      val it = git.log().add(start.getObjectId()).call().iterator()

      var latest: RevCommit = null
      var found = Set.empty[RevCommit]

      while (it.hasNext && found.size < commits.size) {
        val current = it.next()
        if (commits(current)) {
          if (latest == null) latest = current
          found += current
        }
      }
      if (latest != null && commits.size == found.size)
        Some(latest)
      else None
    }

  }

  test("testing new versions conflict management") {

    import EitherValues._

    REMOVEMEusingTempDir { tmpDir =>
      val name = "ivy-version-test"
      val repo = new FileRepository(tmpDir, name)
      val repoDir = repo.repoDir
      println(repoDir.getAbsolutePath())
      Git.init()
        .setDirectory(repoDir)
        .call()
      val git = Git.open(repoDir)

      def publish(variant: Variant) = {
        git.rm().addFilepattern("metadata/A/").call()
        println("file: " + repo.writeVariant(variant).right.value)
        git.add().addFilepattern(".").call()
        git.commit()
          .setMessage(variant.attribute("version").values.mkString(" "))
          .call()
      }

      val variantA1 = Variant(Id("A"), Set(ArtifactRef(Hash("123"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("1.0"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("1.0"))))))
      val variantA2 = Variant(Id("A"), Set(ArtifactRef(Hash("456"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("2.0"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("2.0"))))))
      val variantA3 = Variant(Id("A"), Set(ArtifactRef(Hash("789"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("3.0"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("3.0"))))))

      val commit1 = publish(variantA1)
      val commit3 = publish(variantA3)

      val branchName = variantA2.attribute("version").values.mkString(" ")
      val a = git.branchCreate().setStartPoint(commit1).setName(branchName).call()
      git.checkout().setName(branchName).call()
      val commit2 = publish(variantA2)

      git.checkout().setName("master").call()
      git.merge().setStrategy(MergeStrategy.OURS).include(commit2).setCommit(false).call()
      git.rm().addFilepattern(".").call() //TODO: this is not right...
      git.commit().setMessage("Merge insert of " + variantA2 + " after " + variantA1).call()
      val masterRef = git.checkout().setName("master").call()

      println(commit3)
      println(commit2)
      println(commit1)
      println()

      //git.log().add(masterRef.getObjectId()).call()
      val revWalk = new RevWalk(git.getRepository())
      revWalk.markStart(revWalk.lookupCommit(masterRef.getObjectId))

      //skip merges
      revWalk.setRevFilter(RevFilter.NO_MERGES)
      val it = revWalk.iterator()

      while (it.hasNext) {
        val current = it.next()
        val tree = current.getTree()
        val gitRepo = git.getRepository()
        val treeWalk = new TreeWalk(gitRepo)
        treeWalk.addTree(tree)
        treeWalk.setRecursive(false)
        println("---> " + current)
        while (treeWalk.next()) {
          if (treeWalk.isSubtree()) {
            treeWalk.enterSubtree()
          } else {
            val objectId = treeWalk.getObjectId(0)
            //println(treeWalk.getPathString())
            gitRepo.open(objectId).copyTo(System.out)
            println()
          }
        }
      }

      //      for (_ <- 1 to 1000) {
      //
      //        if (using(git, masterRef).getLatest(Set(commit1, commit2, commit3)) != Some(commit3)) throw new Exception(Set(commit1, commit2, commit3) + "VS" + commit3)
      //        if (using(git, masterRef).getLatest(Set(commit2, commit1, commit3)) != Some(commit3)) throw new Exception(Set(commit2, commit1, commit3) + "VS" + commit3)
      //        if (using(git, masterRef).getLatest(Set(commit3, commit2, commit1)) != Some(commit3)) throw new Exception(Set(commit3, commit2, commit1) + "VS" + commit3)
      //        if (using(git, masterRef).getLatest(Set(commit1, commit2)) != Some(commit2)) throw new Exception(Set(commit1, commit2) + "VS" + commit2)
      //        if (using(git, masterRef).getLatest(Set(commit1, commit3)) != Some(commit3)) throw new Exception(Set(commit1, commit3) + "VS" + commit3)
      //        if (using(git, masterRef).getLatest(Set(commit1)) != Some(commit1)) throw new Exception(Set(commit1) + "VS" + commit1)
      //      }
    }

  }

}