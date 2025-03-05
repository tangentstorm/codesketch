.PHONY: all build test clean install

# Default target
all: build

# Build the project
build:
	cabal build

# Run the tests
test:
	cabal test

# Clean build artifacts
clean:
	cabal clean

# Install the binary
install:
	cabal install

# Run on test file
sample:
	cabal run codesketch -- test/test_rust.rs