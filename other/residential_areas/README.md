TLDR: copy data, downscale it yourself, run `preprocessing.sh`, then `main.sh`

1. Copying the data to the `data` dir (will probably easier in the future)
    a. Go to `Preprocessed.Combined`, find the `residentialProximity` function and change the stepWrapper's argument to `DontRemoveStepDir`
    b. Compile & run an example analysis that uses residential proximity
    c. Copy `residential_reproj.tif` from the internal step dir to `data`

2. Resample (downscale) it yourself using gdalinfo and gdalwarp (otherwise subsequent calculations will take way too long)
    a. For me, `gdalinfo` says the pixel sizes are `(37.479554565173551,-37.479554565173551)`, so I halved it. I think that's how it works.
    b. Therefore I ran `gdalwarp -tr 74.959109130347102 74.959109130347102 './data/residential_reproj.tif' './data/residential_reproj_resample.tif'`

3. Run `preprocessing.sh`, which will:
    a. Vectorizes it (it takes a long time, and will appear to be stuck)
    b. Extract field = 1
    c. Fix geometries (to remove nested polygons)
    d. Simplify geometries using the Douglas-Peucker algorithm with tolerance 100m

4. Finally, generate the buffers with `main.sh` (takes a long time)

5. (TODO) Plot the buffers
