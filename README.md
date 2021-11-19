# site-suitability

- Modular: if one step fails and aborts the program, once you fix it you can simply re-run the program. it will skip calculating existing files and continue as if it was never aborted
- Not a black box: every pre-processing, standardization, and weighting step is given back. (TODO) can also force it to not delete intermediate steps. not a black box where you give it a bunch of things and it gives you an answer. it's interactive as well

# Q&A

- Why not use Python (rasterio) or R?

Because I started work on this before those were taught to me. I'll use them if appropriate, especially for generating figures.

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
-- ghcide automatically generates the signature...
batch = zipWithM
```

- But why Haskell?

Because it is my favourite programming language. I will use Python when appropriate, but Haskell can handle calling CLI commands perfectly

- This code is spaghetti! It's buggy

Apologies, but I have to meet a deadline. Fortunately, Haskell makes refactoring painless

# Installation

**NOTE: YOU MUST HAVE AN UPDATED GDAL VERSION**

Your GDAL version must be at least (aka, oldest possible):

```sh
$ gdalinfo --version
GDAL 3.3.1, released 2021/06/28
```

1. git clone and cd into dir
2. stack build

# Running

**If you're using (ana)conda, please disable it temporarily**
Use `conda deactivate`
If you want to enable it, type `conda activate base`, where base is your environment name

Therefore the command to compile and run would be like:

```
conda deactivate && stack build --fast && stack exec site-suitability
```

# License

(MIT) or (GPLv3 or later), at your choice
