import matplotlib.pyplot as plt
import seaborn as sns


# (note: type signatures do not show the IO monad, so any type here can do IO)
def plot_shared_histograms(
    iterator: '[a]',
    preprocess_item: '(a -> DataFrame float)',
    get_title: '(a -> str)',
    outfile: str,
    xlabel: str = 'Suitability score',
    ncols: int = 2,
    nrows: int = 3
):
    '''
    For every value `A` in iterator `iterator`:
    1) Apply the `preprocess_item` function to `A`, giving a DataFrame
       with a column `'x'` containing floats
    2) Plot a histogram of that column in a separate subplot with a common y-axis
    3) Set the title of the histogram to the `get_title` function applied to `A`,
       which returns a string
    4) Set the xlabel of the histogram to `xlabel`
    5) Save the output to file `outfile`
    '''
    _, axes = plt.subplots(
        ncols=ncols, nrows=nrows,
        sharey=True, gridspec_kw={'wspace': 0}, figsize=(10, 8)
    )
    axes = axes.flatten()

    for idx, f in enumerate(iterator):
        df = preprocess_item(f)
        sns.histplot(x=df['x'], ax=axes[idx])
        axes[idx].set_title(get_title(f))

    axes[0].set_ylabel('Frequency')
    for ax in axes:
        ax.set_xlabel(xlabel)

    plt.tight_layout()
    plt.savefig(outfile)
