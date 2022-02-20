import os
from pathlib import Path
from urllib.request import urlretrieve

# URLS :: {str: [(str, str)]}
URLS = {
    'solar_radiation': [
        ('wc2.1_30s_srad.zip', 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_srad.zip')
    ],
    'elevation': [
        ('srtm_14_05.zip', 'https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_14_05.zip'),
        ('srtm_14_06.zip', 'https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_14_06.zip'),
        ('srtm_15_05.zip', 'https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_15_05.zip'),
        ('srtm_15_06.zip', 'https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_15_06.zip'),
    ],
    'power_lines': [
        ('power_lines.zip', 'https://opendata.arcgis.com/api/v3/datasets/468e9601b9b7407396e5c4f59772f1ff_0/downloads/data?format=shp&spatialRefId=3857')
    ],
    'average_temperature': [
        ('wc2.1_30s_tavg.zip', 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip')
    ],
    'protected_areas': [
        ('WDPA_WDOECM_Feb2022_Public_USA_shp.zip', 'https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_Feb2022_Public_USA_shp.zip')
    ],
    'land_use': [
        ('nlcd_2019_land_cover_l48_20210604.zip', 'https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_land_cover_l48_20210604.zip')
    ],
    'borders': [
        ('gadm36_USA_shp.zip', 'https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_USA_shp.zip')
    ]
}

for criteria_name, urls in URLS.items() :
    for filename, url in urls:
        dir_ = Path(criteria_name)
        dir_.mkdir(parents=True, exist_ok=True)
        path = dir_ / filename
        urlretrieve(url, path)
        # -n: never overwrite files
        cmd = f"unzip -n '{str(path)}' -d '{str(dir_)}'"
        os.system(cmd)
        if criteria_name == 'protected_areas':
            for x in {0, 1, 2}:
                name = f'WDPA_WDOECM_Feb2022_Public_USA_shp_{x}'
                zip_ = dir_ / f'{name}'
                zip_.mkdir(exist_ok=True)
                os.system(f"unzip -n '{str(zip_)}.zip' -d 'protected_areas/{name}'")
