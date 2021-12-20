# Datasets used in this case study

## Borders

www.gadm.org

## Land use (residential areas)

From the 2019 USGS national land cover database:

https://www.mrlc.gov/data?f%5B0%5D=category%3Aland%20cover&f%5B1%5D=year%3A2019
https://www.usgs.gov/centers/eros/science/national-land-cover-database

For the legend key, see:

https://www.mrlc.gov/data/legends/national-land-cover-database-2019-nlcd2019-legend

22, 23, and 24 are considered to be residential here, but in reality it covers non-residential urban areas as well

The data is in `ige`/`rde` format, so it is manually reprojected and cropped to Arizona using the above border file and saved as `land_use_out.tif`.

## Major roads

From OpenStreetMap. Use the overpass turbo API with the following query:

`highway='motorway' or highway='trunk' or highway='primary'`

## Protected areas

www.protectedplanet.net

The October dataset is used, but any future one will suffice.

## Solar radiation (insolation)

https://www.worldclim.org/data/worldclim21.html

As the name suggests, `wc2.1_30s_srad_01.tif` means the spatial resolution is 30 seconds.

## SRTM (elevation)

https://srtm.csi.cgiar.org/download/

Arizona is in the cells `14_05`, `14_6`, `15_05`, and `15_06`

## Power lines

https://hifld-geoplatform.opendata.arcgis.com/datasets/electric-power-transmission-lines?geometry=-107.841%2C30.438%2C-96.471%2C33.695

## Existing solar farms

From OpenStreetMap. Use the overpass turbo API with the following query:

`power=plant and plant:source=solar and plant:method=photovoltaic and plant:output:electricity=*`

(could be better to remove plant:output)

