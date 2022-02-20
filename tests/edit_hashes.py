import json
from PIL import Image
from imagehash import average_hash

with open('./hashes.json', 'r') as f:
    hashes = json.load(f)

Image.MAX_IMAGE_PIXELS = 588152666

file = '../out/suh/std/avg temp_std.tif'
hash_ = average_hash(Image.open(file)).hash.astype(int).tolist()

# Adding or mutating the hash for `file`
hashes[file] = hash_

# Deleting the hash for another file
# del hashes['another_file']

with open('./hashes.json', 'w') as f:
    json.dump(hashes, f)
