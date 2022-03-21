from vis import sigmoid, gaussian, linearL
import numpy as np
import factor

def main():
    # get by running hist_values.r
    breaks = [
        17600, 17700, 17800, 17900, 18000, 18100, 18200, 18300, 18400, 18500, 18600, 18700, 18800, 18900, 19000, 19100, 19200, 19300, 19400, 19500, 19600, 19700, 19800
    ]

    counts = [
        1, 30, 241, 964, 2590, 6993, 11178, 11141, 11419, 12914, 11277, 16326, 21379, 24831, 19402, 23190, 23614, 45650, 42593, 40504, 54723, 29601, 5616
    ]

    xlim = (1000, 21600)
    xlab = 'Solar radiation (kWh/m$^2$/day)'
    x = np.linspace(1000, 21600, 100)
    outfile = 'insolation.png'

    def plot(ax2):
        y1 = sigmoid(2 * 3600, -5)(x)
        y2 = gaussian(6, 4, 3600)(x)
        y3 = list(map(linearL(17691.42, 19875.58), x))

        ax2.plot(x, y1, label='sigmoid(2, -5)')
        ax2.plot(x, y2, label='gaussian(6, 4)')
        ax2.plot(x, y3, label='linearL(17691.42, 19875.58)')

    factor.main(breaks, counts, xlim, xlab, outfile, plot)


if __name__ == '__main__':
    main()
