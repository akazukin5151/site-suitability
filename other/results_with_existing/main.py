# Intended to be ran in the current directory
# (ie, `python main.py` NOT `python histograms/main.py`)

import os
import sys
from pathlib import Path
import pandas as pd
import rasterio
import seaborn as sns
import matplotlib.pyplot as plt

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from plot_shared_histograms import plot_shared_histograms  # nopep8: E402

outfile = 'data/scores_of_existing.csv'
if not Path(outfile).exists():
    existing = pd.read_csv('../existing_solar_farms/existing_solar_farms.csv')
    existing['centroid_x'] = existing['centroid_x'].astype(float)
    existing['centroid_y'] = existing['centroid_y'].astype(float)

    # g :: str -> [float]
    def g(raster):
        a = rasterio.open(raster)
        b = a.read(1)

        # f :: Series float float -> float
        def f(r):
            row, col = a.index(r['centroid_x'], r['centroid_y'])
            return b[row, col]

        return [f(r) for _, r in existing.iterrows()]

    files = [
        '../../out/asakareh/final_clipped.tif',
        '../../out/asakareh_no_res/final_clipped.tif',
        '../../out/watson/final_clipped.tif',
        '../../out/suh/final_clipped.tif',
        '../../out/suh_improved/final_clipped.tif',
        '../../out/suh_range_no_elevation/final_clipped.tif',
        '../../out/suh_range_no_elevation/final_std.tif'
    ]

    # scores :: [[float]]
    # names :: [str]
    scores = [g(file) for file in files]
    names = [Path(file).parent.name for file in files]
    names[-1] = 'suh_range_no_elevation_noclip'

    df = pd.DataFrame(scores, index=names).reset_index()
    df = df.melt('index')
    df.to_csv('data/scores_of_existing.csv')
else:
    df = pd.read_csv(outfile)

df.groupby('index')['value'].describe().to_html('out/descriptive_stats.html')

sns.boxplot(data=df, x='value', y='index')
plt.savefig('out/scores_of_existing_box.png')
plt.close()


# preprocess_item :: DataFrame (str, float) -> DataFrame (str, float)
def preprocess_item(sub_df):
    return sub_df.rename(columns={'value': 'x'})

# get_title :: DataFrame (str, float) -> str
def get_title(sub_df):
    return sub_df['index'].iloc[0]


# iterator :: [DataFrame (str, float)]
# list of dataframes with 'index' and 'value' columns, with a unique 'index' for each df
iterator = [df[df['index'] == x] for x in df['index'].unique()]

plot_shared_histograms(
    iterator, preprocess_item, get_title, 'out/scores_of_existing_hist.png'
)
