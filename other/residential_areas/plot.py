import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib import cm
import rasterio as rio
from rasterio.plot import show

_, ax1 = plt.subplots(figsize=(15, 15))

# The crs of the raster and vectors are both pseudo-mercator, but the
# extents are different...
# using twin axis and resizing to work around it
# manual plots in QGIS doesn't have this problem
ax2 = ax1.twinx().twiny()

with rio.open('data/residential_prox.tif') as f:
    prox = f.read(1)

show(prox, ax=ax1, cmap='Greens')

# manually figured out with trial and error
ax1.set_xlim((-3850, 21000))
ax1.set_ylim((26000, -5580))

cmap = cm.get_cmap('tab10')
dists = list(reversed(['1000', '5000', '10000', '50000']))
colors = cmap(range(len(dists)))

border = gpd.read_file('data/border_reproj.shp')
border.boundary.plot(ax=ax2, lw=5, edgecolor='black')

for color, dist in zip(colors, dists):
    df = gpd.read_file(f'out/{dist}m_dissolve.shp')
    df.boundary.plot(ax=ax2, label=dist, color=color)

legend_elements = [
    Line2D([0], [0], color=color, lw=2, label=dist)
    for color, dist in zip(colors, dists)
]

legend_elements += [Line2D([0], [0], color='black', lw=5, label='Study area')]

ax2.legend(handles=legend_elements)

plt.tight_layout()
plt.savefig('all.png')
