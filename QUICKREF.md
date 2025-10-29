# Quick Reference Card

## Common Commands

| Command | Description |
|---------|-------------|
| `make help` | Show all available commands |
| `make setup` | Check if required tools are installed |
| `make run-scala` | Run Scala program |
| `make run-rust` | Run Rust program |
| `make run-haskell` | Run Haskell program |
| `make test-scala` | Run Scala tests |
| `make test-rust` | Run Rust tests |
| `make test-haskell` | Run Haskell tests |
| `make repl-scala` | Start Scala REPL |
| `make repl-rust` | Start Rust REPL (requires evcxr) |
| `make repl-haskell` | Start Haskell REPL (GHCi) |
| `make clean` | Clean all build artifacts |

## Functional Programming Cheat Sheet

### Map
Transform each element in a collection

**Scala:**
```scala
List(1, 2, 3).map(_ * 2)  // List(2, 4, 6)
```

**Rust:**
```rust
vec![1, 2, 3].iter().map(|x| x * 2).collect()  // [2, 4, 6]
```

**Haskell:**
```haskell
map (*2) [1, 2, 3]  -- [2, 4, 6]
```

### Filter
Keep only elements that match a condition

**Scala:**
```scala
List(1, 2, 3, 4, 5).filter(_ % 2 == 0)  // List(2, 4)
```

**Rust:**
```rust
vec![1, 2, 3, 4, 5].iter().filter(|x| *x % 2 == 0).collect()  // [2, 4]
```

**Haskell:**
```haskell
filter even [1, 2, 3, 4, 5]  -- [2, 4]
```

### Reduce/Fold
Combine all elements into a single value

**Scala:**
```scala
List(1, 2, 3, 4, 5).reduce(_ + _)  // 15
List(1, 2, 3, 4, 5).fold(0)(_ + _)  // 15
```

**Rust:**
```rust
vec![1, 2, 3, 4, 5].iter().sum()  // 15
vec![1, 2, 3, 4, 5].iter().fold(0, |acc, x| acc + x)  // 15
```

**Haskell:**
```haskell
foldr (+) 0 [1, 2, 3, 4, 5]  -- 15
sum [1, 2, 3, 4, 5]  -- 15
```

### Function Composition
Combine functions to create new functions

**Scala:**
```scala
val addOne = (x: Int) => x + 1
val double = (x: Int) => x * 2
val addThenDouble = addOne andThen double
addThenDouble(5)  // 12
```

**Rust:**
```rust
let add_one = |x| x + 1;
let double = |x| x * 2;
let result = double(add_one(5));  // 12
```

**Haskell:**
```haskell
addOne = (+1)
double = (*2)
addThenDouble = double . addOne
addThenDouble 5  -- 12
```

### Pattern Matching

**Scala:**
```scala
def describe(x: Int): String = x match {
  case 0 => "zero"
  case 1 => "one"
  case _ => "other"
}
```

**Rust:**
```rust
fn describe(x: i32) -> &'static str {
    match x {
        0 => "zero",
        1 => "one",
        _ => "other",
    }
}
```

**Haskell:**
```haskell
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe _ = "other"
```

## Key FP Concepts

### Immutability
- **Don't modify data** - create new data instead
- Makes code easier to reason about
- Prevents bugs from unexpected state changes

### Pure Functions
- Same input always gives same output
- No side effects (no I/O, no modifying global state)
- Easy to test and reason about

### Higher-Order Functions
- Functions that take functions as parameters
- Functions that return functions
- Examples: map, filter, reduce

### Lazy Evaluation
- **Haskell**: Everything is lazy by default
- **Scala**: Use `lazy val` or `Stream`/`LazyList`
- **Rust**: Use iterators (they're lazy)

### Algebraic Data Types
- **Scala**: `sealed trait` with case classes
- **Rust**: `enum` with variants
- **Haskell**: `data` declarations

## Learning Path

1. **Start with basics**: map, filter, reduce
2. **Practice immutability**: Avoid var/mut
3. **Use pattern matching**: Replace if-else chains
4. **Learn recursion**: Replace loops
5. **Study monads**: Option/Maybe, Result/Either, IO

## Useful Links

- [Scala Docs](https://docs.scala-lang.org/)
- [Rust Book](https://doc.rust-lang.org/book/)
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Exercism](https://exercism.org/) - Practice with mentorship
