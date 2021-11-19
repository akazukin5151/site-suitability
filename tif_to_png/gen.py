from pathlib import Path
import rasterio as rio
from rasterio.plot import show as rs
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from matplotlib.patches import Patch
import numpy as np


CONSTRAINT_COLORS = 'ListedColormap(["white", "darkgreen"])'
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

    func_name = f"{path.name}_{dir_}"
    code = f"""
def {func_name}():
    cmap = {cmap}

    x = Path('{path / dir_}')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots({nrows}, {ncols}, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)
"""
    if legend is not None:
        code = code + f"""
    plt.legend(
        handles=make_legend({legend}),
        facecolor="white",
    )
"""

    code = code + f"""
    plt.tight_layout()
    plt.savefig('{path}_{dir_}.png')
"""

    print(code)
    return func_name

def do(path: Path):
    a = f(path, 'constraints', CONSTRAINT_COLORS, CONSTRAINT_LEGEND)
    b = f(path, 'preprocessed', '"viridis"')
    c = f(path, 'std', '"viridis"')
    return [a, b, c]

def main():
    print("""
from pathlib import Path
import rasterio as rio
from rasterio.plot import show as rs
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from matplotlib.patches import Patch
import numpy as np

def make_legend(d):
    return [Patch(color=color, label=label)
           for color, label in d.items()]
"""
    )
    res = []
    for f in Path('../out/').iterdir():
        if f.is_dir() and str(f) != '../out/custom':
            res.extend(do(f))
    for x in res:
        print(f'#{x}()')

main()
