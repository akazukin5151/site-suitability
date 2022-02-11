import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

a = pd.read_csv('data/a_rs.csv')
s = pd.read_csv('data/s_rs.csv')
sr = pd.read_csv('data/sr_rs.csv')
w = pd.read_csv('data/w_rs.csv')

_, axes = plt.subplots(ncols=4, sharey=True, gridspec_kw={'wspace': 0}, figsize=(10, 5))
sns.histplot(x=a['x'], ax=axes[0])
sns.histplot(x=s['x'], ax=axes[1])
sns.histplot(x=sr['x'], ax=axes[2])
sns.histplot(x=w['x'], ax=axes[3])
axes[0].set_title('Asakareh')
axes[1].set_title('Suh')
axes[2].set_title('Suh range')
axes[3].set_title('Watson')

axes[0].set_ylabel('Frequency')
for ax in axes:
    ax.set_xlabel('Suitability score')

plt.tight_layout()
plt.savefig('out/hist.png')
