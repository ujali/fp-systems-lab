fn main() {
    println!("Welcome to Rust Learning Lab!");
    println!("This is a playground for learning functional programming in Rust.");
    
    // Example: Basic functional programming concepts
    let numbers = vec![1, 2, 3, 4, 5];
    let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
    println!("Original: {:?}", numbers);
    println!("Doubled: {:?}", doubled);
    
    // Example: Higher-order functions
    let sum: i32 = numbers.iter().sum();
    println!("Sum: {}", sum);
    
    // Example: Closures
    let multiply = |x: i32, y: i32| x * y;
    println!("3 * 4 = {}", multiply(3, 4));
}
