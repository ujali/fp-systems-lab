object Main {
  def main(args: Array[String]): Unit = {
    println("Welcome to Scala Learning Lab!")
    println("This is a playground for learning functional programming in Scala.")
    
    // Example: Basic functional programming concepts
    val numbers = List(1, 2, 3, 4, 5)
    val doubled = numbers.map(_ * 2)
    println(s"Original: $numbers")
    println(s"Doubled: $doubled")
    
    // Example: Higher-order functions
    val sum = numbers.reduce(_ + _)
    println(s"Sum: $sum")
  }
}
