from vis import sigmoid, gaussian, linearL
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

def main():
    # get by running hist_values.r
    breaks = [
        17600, 17700, 17800, 17900, 18000, 18100, 18200, 18300, 18400, 18500, 18600, 18700, 18800, 18900, 19000, 19100, 19200, 19300, 19400, 19500, 19600, 19700, 19800
    ]

    counts = [
        1, 30, 241, 964, 2590, 6993, 11178, 11141, 11419, 12914, 11277, 16326, 21379, 24831, 19402, 23190, 23614, 45650, 42593, 40504, 54723, 29601, 5616
    ]

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
    ax1.set_xlim((1000, 21600))
    ax1.set_ylabel('Frequency')
    ax1.set_xlabel('Solar radiation (kWh/m$^2$/day)')
    #plt.show()
    ax2 = ax1.twinx()

    x = np.linspace(1000, 21600, 100)

    y1 = sigmoid(2 * 3600, -5)(x)
    y2 = gaussian(6, 4, 3600)(x)
    y3 = list(map(linearL(17691.42, 19875.58), x))

    ax2.plot(x, y1, label='sigmoid(2, -5)')
    ax2.plot(x, y2, label='gaussian(6, 4)')
    ax2.plot(x, y3, label='linearL(17691.42, 19875.58)')
    ax2.set_ylabel('Suitability score')
    plt.legend()
    plt.savefig('insolation.png')


if __name__ == '__main__':
    main()
