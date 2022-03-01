# See also
# https://docs.qgis.org/2.18/en/docs/user_manual/plugins/plugins_zonal_statistics.html

import sys
import os
from pathlib import Path
import pandas as pd
import geopandas as gpd

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from plot_shared_histograms import plot_shared_histograms  # nopep8: E402

statistics = ['_sum', '_mean', '_median', '_stdev', '_variety']
input_ = '../existing_solar_farms/existing_solar_farms.shp'

files = [
    '../../out/asakareh/final_clipped.tif',
    '../../out/asakareh_improved/final_clipped.tif',
    '../../out/watson/final_clipped.tif',
    '../../out/suh/final_clipped.tif',
    '../../out/suh_improved/final_clipped.tif',
]

dfs = []

for input_raster in files:
    config_name = Path(input_raster).parent.name
    output = Path('out/shp') / (config_name + '.shp')
    if not output.exists():
        # Count,Sum,Mean,Median,St dev,Minimum,Maximum,Range,Minority,Majority,Variety,Variance
        cmd = f"qgis_process run native:zonalstatisticsfb -- INPUT='{input_}' INPUT_RASTER='{input_raster}' RASTER_BAND=1 STATISTICS='0,1,2,3,4,5,6,7,8,9,10,11' OUTPUT='{output}'"
        os.system(cmd)

    df = gpd.read_file(output)
    tmp = df[statistics]
    tmp['config_name'] = config_name
    dfs.append(tmp)


df = pd.concat(dfs)
df1 = df.melt('config_name')
df1.groupby(['config_name', 'variable'])['value'].describe().to_html(
    'out/descriptive_stats.html'
)
df.groupby('config_name')['_mean'].mean().to_csv('out/mean_of_means.csv')
df.groupby('config_name')['_mean'].std().to_csv('out/std_of_means.csv')

iterator = [x for _, x in df.groupby('config_name')]

def get_title(subdf):
    return subdf.config_name.iloc[0]


for stat in statistics:
    name = stat.lstrip('_')

    def preprocess_item(subdf):
        return subdf.rename(columns={stat: 'x'})

    plot_shared_histograms(
        iterator, preprocess_item, get_title,
        f'out/plots/{name}.png',
        xlabel=name
    )
