dists=(1000 5000 10000 50000)

# nice syntax soup...
for dist in "${dists[@]}"; do

    qgis_process run native:buffer -- INPUT="./out/cropped.shp" OUTPUT="out/${dist}m.shp" DISTANCE="$dist"

    qgis_process run native:dissolve -- INPUT="./out/${dist}m.shp" OUTPUT="./out/${dist}m_dissolve.shp"

done
