# Running Comparison Benchmarks vs Other Language ODE Libraries

This directory contains benchmarks comparing the rgode Rust ODE solver against implementations in other programming languages. These benchmarks help evaluate performance, accuracy, and ease-of-use across different ODE solver libraries.

## Benchmark Structure

### Prerequisites

To run the full set of benchmarks, you'll need:

- Rust (latest stable)
- Fortran compiler (gfortran)
- Python 3.8+ with NumPy, SciPy, and Matplotlib
- Make

### Running Individual Problem Benchmarks

A python script is provided which will run the benchmarks as per the `config.json`. To run the benchmarks:

```bash
python run_benchmarks.py
```

## Interpreting Results

Results can be viewed in the `target/results/` directory which contains json file with the results. From this data plots can be generated as desired.

## Adding New Benchmarks

To add a new benchmark problem:

1. Create a directory under problems/ (e.g., problems/newproblem/)
2. Implement the problem in each target language following the existing structure
3. Setup such that each implementation can be called from its `harnesses/{lang}_harness.py` script
4. Update the `config.json` file with the new problem details