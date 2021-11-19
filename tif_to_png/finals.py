"""
only works for suh
can use map layout for qgis but it's a hell to change symbology
https://opensourceoptions.com/blog/pyqgis-create-and-print-a-map-layout-with-python/
...will do it manually for now
"""
from pathlib import Path
import rasterio as rio
from rasterio.plot import show as rs
import matplotlib.pyplot as plt

def main(path, dir_):
    with rio.open(path / dir_) as f:
        _, ax = plt.subplots()
        rs(f, ax=ax)
        ax.set_title(f'{path.name}_{dir_}')
        plt.tight_layout()
        plt.savefig(f'{path}_{dir_.stem}.png')

main(Path('../out/suh'), Path('final_clipped.tif'))
#for f in Path('../out/').iterdir():
#    if f.is_dir():
#        main(f, Path('final_clipped.tif'))
#        main(f, Path('constraints.tif'))
#        main(f, Path('final_std.tif'))
