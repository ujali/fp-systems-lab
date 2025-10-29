# Contributing to FP Systems Learning Lab

This is a personal learning repository, but the structure and setup can be helpful for others. Here's how to work with this lab effectively.

## Adding New Exercises

### Scala

1. Create new files in `scala/src/main/scala/` for your exercises
2. Create corresponding test files in `scala/src/test/scala/`
3. Run with `make run-scala` or `sbt run` from the scala directory
4. Test with `make test-scala`

Example:
```scala
// scala/src/main/scala/exercises/ListExercises.scala
package exercises

object ListExercises {
  def sumList(list: List[Int]): Int = list.sum
  
  def filterEven(list: List[Int]): List[Int] = 
    list.filter(_ % 2 == 0)
}
```

### Rust

1. Create new modules in `rust/src/` or add to `main.rs`
2. Add test modules with `#[cfg(test)]` and `#[test]` attributes
3. Run with `make run-rust` or `cargo run` from the rust directory
4. Test with `make test-rust`

Example:
```rust
// In rust/src/lib.rs or a new file
pub fn sum_list(list: &[i32]) -> i32 {
    list.iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_sum_list() {
        assert_eq!(sum_list(&[1, 2, 3, 4, 5]), 15);
    }
}
```

### Haskell

1. Create new modules in `haskell/app/` or a `src/` directory
2. Add corresponding test files in `haskell/test/`
3. Update `fp-haskell-lab.cabal` if you add new modules
4. Run with `make run-haskell` or `stack run` from the haskell directory
5. Test with `make test-haskell`

Example:
```haskell
-- haskell/src/Exercises/Lists.hs
module Exercises.Lists where

sumList :: [Int] -> Int
sumList = foldr (+) 0

filterEven :: [Int] -> [Int]
filterEven = filter even
```

## Learning Workflow

### 1. Start with the REPL

Before writing files, experiment in the REPL:
- `make repl-scala` - Explore Scala interactively
- `make repl-rust` - Explore Rust (requires evcxr_repl)
- `make repl-haskell` - Explore Haskell with GHCi

### 2. Write Small Functions

Start with small, focused functions that do one thing well. This is the essence of functional programming.

### 3. Test Your Code

Always write tests for your functions. Tests help you:
- Understand what your code should do
- Catch bugs early
- Document how to use your functions

### 4. Iterate and Refactor

Once your tests pass:
- Look for ways to make your code more functional
- Remove mutable state
- Use higher-order functions
- Practice function composition

## Best Practices

### Scala
- Prefer immutable values (`val`) over mutable ones (`var`)
- Use pattern matching instead of if-else chains
- Leverage the standard library's functional methods (map, filter, fold)
- Practice using for-comprehensions

### Rust
- Embrace ownership and borrowing
- Use iterators and iterator adapters
- Prefer pattern matching
- Use `Option` and `Result` instead of null or exceptions

### Haskell
- Keep functions pure (no side effects)
- Use type signatures to document your functions
- Practice function composition with `.` and `$`
- Learn about monads gradually (they're just abstractions!)

## Resources for Deeper Learning

### Books
- **Scala**: "Functional Programming in Scala" (the red book)
- **Rust**: "Programming Rust" by O'Reilly
- **Haskell**: "Haskell Programming from First Principles"

### Online
- [Exercism](https://exercism.org/) - Practice exercises with mentorship
- [Project Euler](https://projecteuler.net/) - Math problems perfect for FP
- [Advent of Code](https://adventofcode.com/) - Annual coding challenges

### Practice Ideas

1. **Implement classic algorithms** functionally (quicksort, mergesort, etc.)
2. **Data structure exercises** (trees, graphs, etc.)
3. **Parse simple formats** (CSV, JSON-like structures)
4. **Solve Project Euler problems** in all three languages
5. **Implement common abstractions** (Functor, Applicative, Monad)

## Troubleshooting

### Build Issues
```bash
make clean          # Clean all build artifacts
make clean-scala    # Clean just Scala
make clean-rust     # Clean just Rust
make clean-haskell  # Clean just Haskell
```

### Installation Issues
```bash
make setup  # Check what tools are installed and get installation links
```

### Format Your Code
```bash
make format-scala    # Auto-format Scala code
make format-rust     # Auto-format Rust code
make format-haskell  # Format Haskell code (needs hindent)
```

## Happy Learning!

Remember: functional programming is a journey, not a destination. Focus on:
- **Immutability**: Don't modify data, create new data
- **Pure functions**: Same input → same output, no side effects
- **Composition**: Build complex functions from simple ones
- **Higher-order functions**: Functions that take or return functions

Start small, practice regularly, and gradually build your skills!
