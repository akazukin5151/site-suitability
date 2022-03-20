from vis import sigmoid, linearS
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

def main():
    # get by running hist_values.r (remove the first value in breaks)
    breaks = [
        2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,
        50,52,54,56,58,60,62,64,66,68,70,72,74,76
    ]

    counts = [
        22766846,8431926,3884009,2539251,1862485,1456932,1176362,958855,
        783334,629724,495948,382519,291880,216903,156414,112349,
        80214,58650,45103,36291,30580,25295,20852,16638,
        12744,9073,6360,4102,2489,1372,858,479,
        285,134,75,56,4,1,
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
    ax1.set_xlim((0, 80))
    ax1.set_ylabel('Frequency')
    ax1.set_xlabel('Slope (angular degrees)')
    #plt.show()
    ax2 = ax1.twinx()

    x = np.linspace(0, 80, 100)

    y1 = sigmoid(9, 3)(x)
    y2 = list(map(linearS(3, 10), x))

    ax2.plot(x, y1, label='sigmoid(9, 3)')
    ax2.plot(x, y2, label='linearS(3, 10)')
    ax2.set_ylabel('Suitability score')
    plt.legend()
    plt.savefig('slope.png')


if __name__ == '__main__':
    main()
