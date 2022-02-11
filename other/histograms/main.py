# Intended to be ran in the current directory
# (ie, `python main.py` NOT `python histograms/main.py`)

import os
import sys
from pathlib import Path
import pandas as pd

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from plot_shared_histograms import plot_shared_histograms  # nopep8: E402


# get_title :: str -> str
def get_title(f):
    return f.name.replace('.csv', '')


# iterator :: [str]
iterator = sorted(Path('data').iterdir())
plot_shared_histograms(iterator, pd.read_csv, get_title, 'out/hist.png')
