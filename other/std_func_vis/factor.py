from vis import sigmoid, gaussian, linearL
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

def main(
    breaks: 'list[int]',
    counts: 'list[int]',
    xlim: '(int, int)',
    xlab: str,
    outfile: str,
    plot
):
    _, ax1 = plt.subplots(figsize=(10, 8))
    sns.distplot(
        breaks,
        hist_kws={
            "weights": counts,
            "edgecolor": "black"
        },
        bins=len(breaks),
        kde=False,
        ax=ax1
    )
    ax1.set_xlim(xlim)
    ax1.set_ylabel('Frequency')
    ax1.set_xlabel(xlab)
    plt.grid(b=True, which='both', axis='both')
    ax2 = ax1.twinx()

    plot(ax2)
    ax2.set_ylabel('Suitability score')
    plt.legend()
    plt.savefig(outfile)

