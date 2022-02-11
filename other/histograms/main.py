from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

_, axes = plt.subplots(
    ncols=3, nrows=2,
    sharey=True, gridspec_kw={'wspace': 0}, figsize=(10, 8)
)
axes = axes.flatten()

for idx, f in enumerate(sorted(Path('data').iterdir())):
    df = pd.read_csv(f)
    sns.histplot(x=df['x'], ax=axes[idx])
    axes[idx].set_title(f.name.replace('.csv', ''))

axes[0].set_ylabel('Frequency')
for ax in axes:
    ax.set_xlabel('Suitability score')

plt.tight_layout()
plt.savefig('out/hist.png')
