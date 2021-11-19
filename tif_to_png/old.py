from pathlib import Path
import rasterio as rio
from rasterio.plot import show as rs
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from matplotlib.patches import Patch
import numpy as np


CONSTRAINT_COLORS = ListedColormap(["white", "darkgreen"])
CONSTRAINT_LEGEND = {'white': 'Exclude', 'darkgreen': 'Include'}

def make_legend(d):
    return [Patch(color=color, label=label)
           for color, label in d.items()]

def f(path, dir_, cmap, legend=None):
    x = path / dir_
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    if len(imgs) % 3 == 0 or len(imgs) % 5 == 0:
        ncols = 3
        nrows = int(np.ceil(len(imgs) / 3))
    else:
        ncols = 4
        nrows = int(np.ceil(len(imgs) / 4))

    _, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        g(img, axes.flat[idx], cmap)

    if legend is not None:
        plt.legend(
            handles=make_legend(legend),
            facecolor="white",
        )

    plt.tight_layout()
    plt.savefig(f'{path}_{dir_}.png')

def g(img, ax, cmap):
    with rio.open(img) as file_:
        x = file_.read(1)
        ax.set_title(img)
        ax.imshow(x, cmap=cmap)

def do(path: Path):
    f(path, 'constraints', CONSTRAINT_COLORS, CONSTRAINT_LEGEND)
    f(path, 'preprocessed', 'viridis')
    f(path, 'std', 'viridis')
    #path / 'constraints.tif'
    #g(path / 'final.tif')
    #g(path / 'final_clipped.tif')

def main():
    for f in Path('.').iterdir():
        if f.is_dir():
            do(f)

main()
