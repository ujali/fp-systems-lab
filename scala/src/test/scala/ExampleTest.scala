import org.scalatest.funsuite.AnyFunSuite

class ExampleTest extends AnyFunSuite {
  test("basic arithmetic") {
    assert(2 + 2 == 4)
  }
  
  test("list operations") {
    val numbers = List(1, 2, 3, 4, 5)
    val doubled = numbers.map(_ * 2)
    assert(doubled == List(2, 4, 6, 8, 10))
  }
  
  test("higher-order functions") {
    val numbers = List(1, 2, 3, 4, 5)
    val sum = numbers.reduce(_ + _)
    assert(sum == 15)
  }
}
