import pytest

import json
import logging
from pathlib import Path

import numpy as np
from PIL import Image
from imagehash import ImageHash, average_hash


LOGGER = logging.getLogger(__name__)
DIRS = {'asakareh_improved', 'asakareh', 'suh', 'suh_improved', 'watson'}
# recursively iterate through subdirectories, hashing every tif
ITERATOR = [
    file
    for dir_ in Path('../out/').iterdir()
    if dir_.name in DIRS
    for file in dir_.rglob("*")
    if file.name.endswith('.tif')
]


@pytest.fixture
def setup_PIL():
    Image.MAX_IMAGE_PIXELS = 588152666

@pytest.fixture(scope='session')
def load_hashes():
    if not Path('hashes.json').exists():
        Image.MAX_IMAGE_PIXELS = 588152666
        hashes = {
            str(file): average_hash(Image.open(file)).hash.astype(int).tolist()
            for file in ITERATOR
        }
        with open('hashes.json', 'w') as f:
            json.dump(hashes, f)

        return hashes

    with open('./hashes.json', 'r') as f:
        return json.load(f)


@pytest.mark.parametrize('file', ITERATOR)
def test_raster_output_are_similar(setup_PIL, load_hashes, file):
    hash1 = average_hash(Image.open(file))
    original_hash = ImageHash(np.array(load_hashes[str(file)]).astype(bool))
    diff = original_hash - hash1
    LOGGER.info(f'Diff for {file} is {diff}')
    assert diff <= 10
