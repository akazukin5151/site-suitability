gdalwarp -overwrite -t_srs EPSG:4326 -of GTiff -cutline "borders/study_area.shp" -crop_to_cutline "land_use/nlcd_2019_land_cover_l48_20210604.img" "land_use/land_use_out.tif"
