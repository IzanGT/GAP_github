# GAP Refined McKay Conjecture Verification

This repository contains the necessary files to execute the computational verification of the refined McKay conjecture for finite simple groups (sporadic and non-sporadic), as described in the associated paper.

## Directory Structure

- `main.g`: Entry point for the framework. Contains the `Classify` function.
- `funciones/`: Contains the GAP modules with the mathematical algorithms and utilities required for the computations.
- `test_logs/`: Contains precomputed logs and summaries of runs on the groups, providing a reference for expected outputs and results without needing to rerun expensive computations.

## Prerequisites

- [GAP (Groups, Algorithms, Programming)](https://www.gap-system.org/) (Version 4.13 or newer recommended).
- The `CTblLib` package for GAP must be installed.

## Usage Instructions

To use the code, open a terminal or command prompt, navigate to the directory where this `README.md` is located, and launch GAP from here.

```gap
gap> ChangeDirectoryCurrent("/path/to/GAP_Paper_Code");
```

Once inside the GAP prompt, you can load the main module and run the classifications.

```gap
gap> Read("main.g");

# View supported sporadic and non-sporadic groups
gap> ShowSporadicSupportList();
gap> ShowNonSporadicSupportList();

# Run the algorithm for a specific group and primes
# Classify( name_S, prime_list, clifford_verification, show_progress, show_tables )
gap> Classify("A5", [2, 3, 5], true, true, false);
```

## Documentation

For a detailed explanation of the logic behind the classification (using the library vs building the groups and their characters explicitly) and the mathematical foundation, please refer to the corresponding paper.

