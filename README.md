
# Build Instructions

1. Install `ghcup`
2. Install the required GHC version:
	 ```bash
	 ghcup install ghc 9.12.2
	 ```
3. Install the required Cabal version:
	 ```bash
	 ghcup install cabal 3.14.2.0
	 ```
4. Build the project:
	 ```bash
	 cabal build
	 ```
5. Run the build:
	 ```bash
	 cabal run -- -h
	 ```

# Run instructions

see help of the builds 
```bash
# help of the command
cabal run -- -h
# help of sub commands
cabal run -- <SUB-CMD> -h 
```