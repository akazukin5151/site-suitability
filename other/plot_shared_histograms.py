import matplotlib.pyplot as plt
import seaborn as sns


# plot_shared_histograms :: [a] -> (a -> DataFrame float) -> (a -> str) -> str -> ()
# (note: type signatures do not show the IO monad, so any type here can do IO)
def plot_shared_histograms(iterator, preprocess_item, get_title, outfile):
    '''
    For every value `A` in iterator `i`:
    1) Apply the `preprocess_item` function to `A`, giving a DataFrame
       with a column `'x'` containing floats
    2) Plot a histogram of that column in a separate subplot with a common y-axis
    3) Set the title of the histogram to the `get_title` function applied to `A`,
       which returns a string
    4) Save the output to file `outfile`
    '''
    _, axes = plt.subplots(
        ncols=3, nrows=3,
        sharey=True, gridspec_kw={'wspace': 0}, figsize=(10, 8)
    )
    axes = axes.flatten()

    for idx, f in enumerate(iterator):
        df = preprocess_item(f)
        sns.histplot(x=df['x'], ax=axes[idx])
        axes[idx].set_title(get_title(f))

    axes[0].set_ylabel('Frequency')
    for ax in axes:
        ax.set_xlabel('Suitability score')

    plt.tight_layout()
    plt.savefig(outfile)
