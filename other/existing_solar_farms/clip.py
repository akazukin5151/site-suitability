import geopandas as gpd

gdf = gpd.read_file('../../../data/existing solar farms/osm/arizona_solar.geojson')

az = gpd.read_file('../../data/borders/study_area.shp')

print(az.crs)
assert gdf.crs == az.crs

gdf_clipped = gdf.clip(az)
gdf_clipped.to_file('./existing_solar_farms.shp')
