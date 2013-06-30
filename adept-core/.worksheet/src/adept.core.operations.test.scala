package adept.core.operations

object test {
trait Animal

trait Bird extends Animal {
  def fly: String = "I am flying!"
}

trait Swimmer {
  def swim: String = "I am swimming!"
}

class Fish extends Animal with Swimmer

class Duck extends Bird with Swimmer {
  super.fly
};import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(292); val res$0 = 

(new Duck).sound;System.out.println("""res0: <error> = """ + $show(res$0))}


}