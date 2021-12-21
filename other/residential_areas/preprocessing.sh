gdal_polygonize.py "data/residential_reproj_resample.tif" -b 1 -f "ESRI Shapefile" "data/residential_reproj_vec.shp" vec DN

qgis_process run native:extractbyattribute -- INPUT='./data/residential_reproj_vec.shp' FIELD='DN' OPERATOR=0 VALUE='1' OUTPUT='./data/vec_extract.shp'

qgis_process run native:fixgeometries -- INPUT='./data/vec_extract.shp' OUTPUT='./data/vec_extract_fix.shp'

# method 0 = 'Distance (Douglas-Peucker)'
qgis_process run native:simplifygeometries -- INPUT='./data/vec_extract_fix.shp' METHOD=0 TOLERANCE=100 OUTPUT='./data/vec_extract_fix_simplify.shp'

# This finishes successfully but QGIS refuses to view it for me...
#qgis_process run native:dissolve -- INPUT="./data/vec_extract_fix_simplify.shp" OUTPUT="./data/vec_extract_fix_simplify_dissolve.shp"
