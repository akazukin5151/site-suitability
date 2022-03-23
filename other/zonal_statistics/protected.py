# See also
# https://docs.qgis.org/2.18/en/docs/user_manual/plugins/plugins_zonal_statistics.html

import sys
import os
from pathlib import Path
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from plot_shared_histograms import plot_shared_histograms  # nopep8: E402

statistics = ['_sum', '_mean', '_median', '_stdev', '_variety']
input_ = '../existing_solar_farms/existing_solar_farms.shp'

files = [
    '../../out/watson/final_clipped.tif',
    '../../out/watson_protected_0/final_clipped.tif',
    '../../out/watson_protected_0.016666666666666666/final_clipped.tif',
    '../../out/watson_protected_0.03333333333333333/final_clipped.tif',
    '../../out/watson_protected_0.05/final_clipped.tif',
    '../../out/watson_protected_0.1/final_clipped.tif',
    '../../out/watson_protected_0.15000000000000002/final_clipped.tif',
    '../../out/watson_protected_0.16666666666666666/final_clipped.tif',
    '../../out/watson_protected_0.18333333333333335/final_clipped.tif',
    '../../out/watson_protected_0.19/final_clipped.tif',
    '../../out/watson_protected_0.2/final_clipped.tif',
]

dfs = []

for input_raster in files:
    config_name = Path(input_raster).parent.name
    output = Path('out/protected/shp') / (config_name + '.shp')
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
    'out/protected/descriptive_stats.html'
)
mean_of_means = df.groupby('config_name')['_mean'].mean()
mean_of_means.to_csv('out/protected/mean_of_means.csv')
mean_of_means = pd.DataFrame(mean_of_means).assign(
    weight=[0.134, 0, 0.016, 0.03, 0.05, 0.1, 0.15, 0.16, 0.18, 0.19, 0.2]
)
mean_of_means.sort_values('weight').set_index('weight').plot.bar()
plt.savefig('out/protected/plots/weights.png')
plt.close()
df.groupby('config_name')['_mean'].std().to_csv('out/protected/std_of_means.csv')

iterator = [x for _, x in df.groupby('config_name')]

def get_title(subdf):
    return subdf.config_name.iloc[0]


for stat in statistics:
    name = stat.lstrip('_')

    def preprocess_item(subdf):
        return subdf.rename(columns={stat: 'x'})

    plot_shared_histograms(
        iterator, preprocess_item, get_title,
        f'out/protected/plots/{name}.png',
        xlabel=name,
        nrows=5, ncols=3,
    )
