# FP Systems Learning Lab

A playground repository for learning and practicing functional programming in Scala, Rust, and Haskell.

## Overview

This repository is organized into three main directories, each dedicated to a different programming language:
- **scala/** - Scala projects and exercises
- **rust/** - Rust projects and exercises  
- **haskell/** - Haskell projects and exercises

## Prerequisites

Make sure you have the following tools installed:

- **Scala**: [SBT](https://www.scala-sbt.org/download.html) (Scala Build Tool)
- **Rust**: [Rustup](https://rustup.rs/) (Rust toolchain installer)
- **Haskell**: [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (Haskell Tool Stack)

To check if you have all required tools installed, run:
```bash
make setup
```

## Quick Start

Run any language's main program:

```bash
# Run Scala
make run-scala

# Run Rust
make run-rust

# Run Haskell
make run-haskell
```

## Available Commands

View all available commands:
```bash
make help
```

### Running Programs

```bash
make run-scala      # Run Scala program (cd scala && sbt run)
make run-rust       # Run Rust program (cd rust && cargo run)
make run-haskell    # Run Haskell program (cd haskell && stack run)
```

### Testing

```bash
make test-scala     # Run Scala tests
make test-rust      # Run Rust tests
make test-haskell   # Run Haskell tests
```

### Building

```bash
make build-scala    # Build Scala project
make build-rust     # Build Rust project
make build-haskell  # Build Haskell project
```

### Code Formatting

```bash
make format-scala   # Format Scala code with scalafmt
make format-rust    # Format Rust code with rustfmt
make format-haskell # Format Haskell code with hindent
```

### Interactive REPLs

For interactive exploration and experimentation:

```bash
make repl-scala     # Start Scala REPL (sbt console)
make repl-rust      # Start Rust REPL (requires evcxr_repl)
make repl-haskell   # Start Haskell REPL (stack ghci)
```

### Cleaning Build Artifacts

```bash
make clean-scala    # Clean Scala build artifacts
make clean-rust     # Clean Rust build artifacts
make clean-haskell  # Clean Haskell build artifacts
make clean          # Clean all build artifacts
```

## Project Structure

```
fp-systems-lab/
├── scala/
│   ├── build.sbt              # SBT build configuration
│   ├── project/
│   │   └── build.properties   # SBT version
│   └── src/
│       └── main/
│           └── scala/
│               └── Main.scala # Main entry point
├── rust/
│   ├── Cargo.toml             # Rust project configuration
│   └── src/
│       └── main.rs            # Main entry point
├── haskell/
│   ├── stack.yaml             # Stack configuration
│   ├── fp-haskell-lab.cabal   # Cabal package description
│   └── app/
│       └── Main.hs            # Main entry point
├── Makefile                   # Build automation
└── README.md                  # This file
```

## Learning Resources

### Scala
- [Scala Official Documentation](https://docs.scala-lang.org/)
- [Scala Exercises](https://www.scala-exercises.org/)
- [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala)

### Rust
- [The Rust Book](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Rustlings - Small exercises](https://github.com/rust-lang/rustlings)

### Haskell
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Haskell Programming from First Principles](https://haskellbook.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)

## Tips for Learning

1. **Start with the REPL**: Use `make repl-<language>` to experiment with code interactively
2. **Run frequently**: Use the `make run-*` commands to see your changes in action
3. **Test your code**: Write tests as you learn to validate your understanding
4. **Format regularly**: Use `make format-*` to keep your code clean and readable
5. **Clean when stuck**: Use `make clean` if you encounter build issues

## Adding New Exercises

Simply create new files in the respective `src/` directories for each language. The build tools will automatically include them in the compilation process.

## License

This is a personal learning repository. Feel free to use it as a template for your own learning journey!