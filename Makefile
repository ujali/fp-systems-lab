.PHONY: help run-scala run-rust run-haskell test-scala test-rust test-haskell clean-scala clean-rust clean-haskell clean format-scala format-rust format-haskell setup

# Default target - show help
help:
	@echo "FP Systems Learning Lab - Available Commands:"
	@echo ""
	@echo "Running Programs:"
	@echo "  make run-scala      - Run Scala program"
	@echo "  make run-rust       - Run Rust program"
	@echo "  make run-haskell    - Run Haskell program"
	@echo ""
	@echo "Testing:"
	@echo "  make test-scala     - Run Scala tests"
	@echo "  make test-rust      - Run Rust tests"
	@echo "  make test-haskell   - Run Haskell tests"
	@echo ""
	@echo "Building:"
	@echo "  make build-scala    - Build Scala project"
	@echo "  make build-rust     - Build Rust project"
	@echo "  make build-haskell  - Build Haskell project"
	@echo ""
	@echo "Formatting:"
	@echo "  make format-scala   - Format Scala code"
	@echo "  make format-rust    - Format Rust code"
	@echo "  make format-haskell - Format Haskell code"
	@echo ""
	@echo "Cleaning:"
	@echo "  make clean-scala    - Clean Scala build artifacts"
	@echo "  make clean-rust     - Clean Rust build artifacts"
	@echo "  make clean-haskell  - Clean Haskell build artifacts"
	@echo "  make clean          - Clean all build artifacts"
	@echo ""
	@echo "Interactive REPLs:"
	@echo "  make repl-scala     - Start Scala REPL"
	@echo "  make repl-rust      - Start Rust REPL (requires evcxr)"
	@echo "  make repl-haskell   - Start Haskell REPL (GHCi)"
	@echo ""
	@echo "Setup:"
	@echo "  make setup          - Check if all required tools are installed"

# Run commands
run-scala:
	cd scala && sbt run

run-rust:
	cd rust && cargo run

run-haskell:
	cd haskell && stack run

# Test commands
test-scala:
	cd scala && sbt test

test-rust:
	cd rust && cargo test

test-haskell:
	cd haskell && stack test

# Build commands
build-scala:
	cd scala && sbt compile

build-rust:
	cd rust && cargo build

build-haskell:
	cd haskell && stack build

# Format commands
format-scala:
	cd scala && sbt scalafmt

format-rust:
	cd rust && cargo fmt

format-haskell:
	@echo "Note: Install ormolu or brittany for Haskell formatting"
	@echo "For now, using hindent if available..."
	@command -v hindent >/dev/null 2>&1 && find haskell/app -name "*.hs" -exec hindent {} \; || echo "hindent not found. Install with: stack install hindent"

# Clean commands
clean-scala:
	cd scala && sbt clean
	rm -rf scala/target scala/project/target scala/project/project

clean-rust:
	cd rust && cargo clean

clean-haskell:
	cd haskell && stack clean

clean: clean-scala clean-rust clean-haskell

# REPL commands
repl-scala:
	cd scala && sbt console

repl-rust:
	@command -v evcxr >/dev/null 2>&1 && evcxr || echo "Rust REPL not found. Install with: cargo install evcxr_repl"

repl-haskell:
	cd haskell && stack ghci

# Setup check
setup:
	@echo "Checking for required tools..."
	@echo ""
	@echo -n "Scala (sbt): "
	@command -v sbt >/dev/null 2>&1 && echo "✓ Installed" || echo "✗ Not found - Install from https://www.scala-sbt.org/"
	@echo -n "Rust (cargo): "
	@command -v cargo >/dev/null 2>&1 && echo "✓ Installed" || echo "✗ Not found - Install from https://rustup.rs/"
	@echo -n "Haskell (stack): "
	@command -v stack >/dev/null 2>&1 && echo "✓ Installed" || echo "✗ Not found - Install from https://docs.haskellstack.org/"
	@echo ""
	@echo "Optional tools:"
	@echo -n "Rust REPL (evcxr): "
	@command -v evcxr >/dev/null 2>&1 && echo "✓ Installed" || echo "✗ Not found - Install with: cargo install evcxr_repl"
	@echo -n "Haskell formatter (hindent): "
	@command -v hindent >/dev/null 2>&1 && echo "✓ Installed" || echo "✗ Not found - Install with: stack install hindent"
