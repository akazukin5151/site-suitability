# Integration tests

It uses [imagehash](https://github.com/JohannesBuchner/imagehash/) to generate hashes (similar images have similar hashes) to compare current outputs in `../out/<CONFIG-NAME>/` with a baseline, which represents the "correct" result. Large differences will cause the test to fail.

## Setup

Install [imagehash](https://github.com/JohannesBuchner/imagehash/) and pytest

Remember all shell commands below are in this directory (`tests`), not the repo's root directory

Remember that the datasets can be downloaded and processed exactly, see `../data/`

## Usage
### Comparing current output to examples in ../configs

1. Run some configs in `../configs`, so that `../out/<CONFIG-NAME>/` and sub-directories has a bunch of `.tif` files
2. Run tests with `pytest -vvvv --log-cli-level=INFO test.py`

### Ad-hoc comparisons

Scenario: you made a small edit to an existing config and want to test if a few specific images have changed

1. Modify the files list in `adhoc.py` according to whatever files you want to test
2. Run `python adhoc.py`

### Using a different baseline

Scenario: you wrote a new config, or modified an existing config and want the current result in `../out/<CONFIG-NAME>/` to be the baseline

1. Delete `hashes.json`
2. Edit `test.py` on this line to reflect the configs whose hashes you want to be saved (so that the `../out` directory can have any other sub-directories containing `.tif` files, without interfering with the tests)

```py
DIRS = {'asakareh_improved', 'asakareh', 'suh', 'suh_improved', 'watson', 'your_config_here'}
```

2. Run all the configs that you want to store as a baseline
3. Run tests as above. As `hashes.json` is missing, it will re-calculate the imagehashes of every `.tif` file in `../out/<CONFIG>` recursively and store it in `hashes.json`, for all CONFIGS specified in step 2

`hashes.json` is a simple key-value mapping between `tif`-path and imagehash, so every subsequent imagehash access is just a O(1) lookup

### Editing the current baseline

Edit the `file` variable in `edit_hashes.py` and run it
