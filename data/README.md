1. Run `python download_simple.py`. This will download datasets that can be accessed in simple URLs and unzip them to their respective directories
2. Run `python download_roads.py`. This will use OpenStreetMap's overpass API to download the major roads dataset
3. Run `./crop_border.sh`. This will extract Arizona's borders from the GADM US state borders. This is the study area.
4. Run `./land_use.sh`. This will crop `land_use/nlcd_2019_land_cover_l48_20210604.img` to the study area (generated in step 3) and reproject to WGS 84
