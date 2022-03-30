# site-suitability

Program to automate site suitability / site selection analysis

- Modular: if one step fails and aborts the program, once you fix it you can simply re-run the program. it will skip calculating existing files and continue as if it was never aborted
- Not a black box: every pre-processing, standardization, and weighting step is given back. not a black box where you give it a bunch of things and it gives you an answer.

# Installation

## Prerequisites

- Install [Haskell Stack](https://github.com/commercialhaskell/stack/)
- Install an updated version of GDAL

Your GDAL version must be at least (aka, cannot be older than this):

```sh
$ gdalinfo --version
GDAL 3.3.1, released 2021/06/28
```

For example commands, see [example_ci.yml](example_ci.yml)

## Compile

1. git clone and cd into dir
2. stack build

# Usage

## Prerequisites

Download the datasets and do some slight pre-processing. See [README_datasets.md](README_datasets.md) and [data/README.md](./data/README.md).

TLDR: run this

```sh
cd data
python download_simple.py
python download_roads.py
./crop_border.sh
./land_use.sh
```

## Running

(Optional) edit `configs/run.txt`. Put the names of the configs you want to run on a new line. Do not type extensions (`.json`)

**If you're using (ana)conda, please disable it temporarily**
Use `conda deactivate`
(To enable it back, type `conda activate base`, where base is your environment name)

Therefore the command to compile and run would be like:

```
conda deactivate && stack build && stack exec site-suitability
```

# Integration tests

Install [pytest](https://docs.pytest.org/en/6.2.x/) and imagehash (`pip install imagehash`). Run the program at least once to generate outputs, then run:

```py
cd tests
pytest -vvvv --log-cli-level=INFO test.py
```

This will calculate and store similarity-hashes of all outputs and save them as the baseline. Future runs will test existing outputs against the stored baseline

To customize which configs are saved, see `tests/README.md`

# Q&A

- Why not use Python (rasterio) or R?

Because I started work on this before those were taught to me. They're already used to generate figures.

- Why not Rust?

Because this is the signature of the first function I ported:

```rs
pub fn batch<A, B>(
    f: fn((<A as Iterator>::Item, <A as Iterator>::Item)) -> B,
    input_files: A,
    output_files: A,
) // -> B // doesn't even work!
where
    A: Iterator
```

Compared to Haskell:

```hs
-- Haskell language server automatically generates the signature
batch = zipWithM
```

- But why Haskell?

This program is just a fancy wrapper around a series of command line instructions anyway, any programming language can do this, so I picked my favourite programming language.

# License

(MIT) or (GPLv3 or later), at your choice
