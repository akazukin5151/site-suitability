import geopandas as gpd
import matplotlib.pyplot as plt

gdf = gpd.read_file('../../../data/existing solar farms/osm/arizona_solar.geojson')

az = gpd.read_file('../../data/borders/study_area.shp')

print(az.crs)
assert gdf.crs == az.crs

gdf_clipped = gdf.clip(az)
gdf_clipped = gdf_clipped.to_crs(26950)  # NAD83 / Arizona West
gdf_clipped['centroid'] = gdf_clipped.centroid
gdf_clipped.to_crs(az.crs)
gdf_clipped['centroid'] = gdf_clipped['centroid'].to_crs(az.crs)
# it can't export a geometry type for some reason...
#gdf_clipped.to_file('out.shp')
gdf_clipped['centroid_x'] = gdf_clipped['centroid'].apply(lambda x: x.x)
gdf_clipped['centroid_y'] = gdf_clipped['centroid'].apply(lambda x: x.y)
gdf_clipped[['id', 'centroid_x', 'centroid_y']].to_csv('existing_solar_farms.csv')

ax = az.plot()
gdf_clipped['centroid'].plot(ax=ax, color='red', markersize=5)
plt.tight_layout()
plt.savefig('existing_farms.png')
