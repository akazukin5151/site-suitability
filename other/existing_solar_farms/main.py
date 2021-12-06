import geopandas as gpd
import matplotlib.pyplot as plt

gdf = gpd.read_file('../../../data/existing solar farms/osm/arizona_solar.geojson')

az = gpd.read_file('../../out/suh/az border.shp')

assert gdf.crs == az.crs

gdf_clipped = gdf.clip(az)
gdf_clipped = gdf_clipped.to_crs(26950)  # NAD83 / Arizona West
gdf_clipped['centroid'] = gdf_clipped.centroid
gdf_clipped.to_crs(az.crs)
gdf_clipped['centroid'] = gdf_clipped['centroid'].to_crs(az.crs)

ax = az.plot()
gdf_clipped['centroid'].plot(ax=ax, color='red', markersize=5)
plt.tight_layout()
plt.savefig('existing_farms.png')
