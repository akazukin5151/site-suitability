
from pathlib import Path
import rasterio as rio
from rasterio.plot import show as rs
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from matplotlib.patches import Patch
import numpy as np

def make_legend(d):
    return [Patch(color=color, label=label)
           for color, label in d.items()]


def asakareh_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/asakareh/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/asakareh_constraints.png')


def asakareh_preprocessed():
    cmap = "viridis"

    x = Path('../out/asakareh/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/asakareh_preprocessed.png')


def asakareh_std():
    cmap = "viridis"

    x = Path('../out/asakareh/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/asakareh_std.png')


def suh_no_constraint_100_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/suh_no_constraint_100/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(0, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/suh_no_constraint_100_constraints.png')


def suh_no_constraint_100_preprocessed():
    cmap = "viridis"

    x = Path('../out/suh_no_constraint_100/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_no_constraint_100_preprocessed.png')


def suh_no_constraint_100_std():
    cmap = "viridis"

    x = Path('../out/suh_no_constraint_100/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_no_constraint_100_std.png')


def suh_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/suh/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/suh_constraints.png')


def suh_preprocessed():
    cmap = "viridis"

    x = Path('../out/suh/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_preprocessed.png')


def suh_std():
    cmap = "viridis"

    x = Path('../out/suh/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_std.png')


def watson_no_protected_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/watson_no_protected/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/watson_no_protected_constraints.png')


def watson_no_protected_preprocessed():
    cmap = "viridis"

    x = Path('../out/watson_no_protected/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/watson_no_protected_preprocessed.png')


def watson_no_protected_std():
    cmap = "viridis"

    x = Path('../out/watson_no_protected/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/watson_no_protected_std.png')


def watson_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/watson/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/watson_constraints.png')


def watson_preprocessed():
    cmap = "viridis"

    x = Path('../out/watson/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/watson_preprocessed.png')


def watson_std():
    cmap = "viridis"

    x = Path('../out/watson/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/watson_std.png')


def suh_no_constraint_500_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/suh_no_constraint_500/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(0, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/suh_no_constraint_500_constraints.png')


def suh_no_constraint_500_preprocessed():
    cmap = "viridis"

    x = Path('../out/suh_no_constraint_500/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_no_constraint_500_preprocessed.png')


def suh_no_constraint_500_std():
    cmap = "viridis"

    x = Path('../out/suh_no_constraint_500/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_no_constraint_500_std.png')


def suh_no_elevation_constraints():
    cmap = ListedColormap(["white", "darkgreen"])

    x = Path('../out/suh_no_elevation/constraints')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(1, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.legend(
        handles=make_legend({'white': 'Exclude', 'darkgreen': 'Include'}),
        facecolor="white",
    )

    plt.tight_layout()
    plt.savefig('../out/suh_no_elevation_constraints.png')


def suh_no_elevation_preprocessed():
    cmap = "viridis"

    x = Path('../out/suh_no_elevation/preprocessed')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 4, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_no_elevation_preprocessed.png')


def suh_no_elevation_std():
    cmap = "viridis"

    x = Path('../out/suh_no_elevation/std')
    imgs = [x for x in x.iterdir() if x.suffix == '.tif']
    _, axes = plt.subplots(2, 3, figsize=(15, 10))
    for idx, img in enumerate(imgs):
        ax = axes.flat[idx]
        with rio.open(img) as file_:
            x = file_.read(1)
            ax.set_title(img.name)
            ax.imshow(x, cmap=cmap)

    plt.tight_layout()
    plt.savefig('../out/suh_no_elevation_std.png')

#asakareh_constraints()
#asakareh_preprocessed()
#asakareh_std()
#suh_no_constraint_100_constraints()
#suh_no_constraint_100_preprocessed()
#suh_no_constraint_100_std()
#suh_constraints()
#suh_preprocessed()
#suh_std()
#watson_no_protected_constraints()
#watson_no_protected_preprocessed()
#watson_no_protected_std()
#watson_constraints()
#watson_preprocessed()
#watson_std()
#suh_no_constraint_500_constraints()
#suh_no_constraint_500_preprocessed()
#suh_no_constraint_500_std()
#suh_no_elevation_constraints()
#suh_no_elevation_preprocessed()
#suh_no_elevation_std()
