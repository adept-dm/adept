package adept.git

import org.scalatest.FunSuite
import org.scalatest.Matchers
import java.io.File
import adept.repository.Commit

class GitWrapperTest extends FunSuite with Matchers {

  //e11eda3 foo 2
  //ad81b33 foo 1
  //ea85afe foo 0
  
  test("read from specific commit") {
    val dir = new File("/Users/freekh/tmp/adept-git-wrapper-tests/t1")
    GitWrapper(dir).read(Commit("ad81b33"), Set("foo1.json")){ (path, reader) =>
      
    }
  }
  
  test("write on specific commit") {
    pending
  }
  
  test("read and update on a specific commit") {
    pending
  }

  test("multiple writes") {
    pending
  }

  test("write locks") {
    pending
  }
  


}