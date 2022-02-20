import json
import numpy as np
from PIL import Image
from imagehash import ImageHash, average_hash

with open('./hashes.json', 'r') as f:
    hashes = json.load(f)

Image.MAX_IMAGE_PIXELS = 588152666

def main(file):
    original_hash = ImageHash(np.array(hashes[file]).astype(bool))
    hash1 = average_hash(Image.open(file))
    diff = original_hash - hash1
    print(f'diff for {file=} is {diff}')


files = [
    '../out/suh_improved/final_clipped.tif'
#    '../out/suh_improved/constraints/residential_constraint.tif',
#    '../out/suh_improved/constraints.tif'
]

[main(file) for file in files]
