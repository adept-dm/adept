package adept.repository

import org.scalatest._
import org.eclipse.jgit.api.Git
import adept.core.models._
import org.eclipse.jgit.merge.MergeStrategy
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.revwalk.filter.RevFilter
import org.eclipse.jgit.treewalk.TreeWalk

class WedgeTest extends FunSuite with Matchers {
  import adept.test.TestHelpers._

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