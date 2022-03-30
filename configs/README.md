# Configs

## Running configurations

Edit `run.txt` to tell the program which config files to run. Put one name on each line. The names should not have file extensions

The `run.txt` file in this repo is a static example for reference

## Writing a configuration

The configuration files are in JSON format. At the top level, there are three keys:

- `"study_area"`: See [Study area](#Study-area)
- `"criteria"`: See [criteria](#Criteria)
- `"constraints"`: See [constraints](#Constraints)

### Study area

An object with only one key:

- `"file"`: path `string` (Vector)
    - The path to the study area vector

### Criteria

(Note: this is more accurately described as factors; criteria for constraints are in the constraints section)

There are three steps when calculating factors:

1. Process input layer(s) into a single Raster layer
2. Standardize the Raster layer so that the values range from 0 to 1
3. Multiply the layer with the weight

A list of objects with the following keys:

- `"name"`: any `string`
    - The name of the factor (can be anything)
- `"inputs"`: `List string` (Vector or Raster)
    - The paths to the input files
- `"output"`: `string`
    - The name of the preprocessed output (output of step 1)
- `"prep_f"`: `PrepFunctions`
    - See [PrepFunctions](#PrepFunctions)
- `"std_f"`: `StdFunctions`
    - See [StdFunctions](#StdFunctions)
- `"weight"`: `number`
    - The weight of the layer, from 0 to 1 inclusive, where 1 is the most important

Optional keys:

- `"require"`: See [Require](#Require)

### Require

This represents a layer that is processed like step 1 of constraints, but not the other steps, and is not included in the final suitability map. Essentially it is just an intermediate step to calculating something else

- `"r_name"`: any `string`
    - Name of the layer (can be anything)
- `"r_inputs"`: `List string` (Vector or Raster)
    - Paths to the files to be processed
- `"r_output"`: `string`
    - The name of the preprocessed output (output of step 1)
- `"r_prep_f"`: `PrepFunctions`
    - See [PrepFunctions](#PrepFunctions)


### PrepFunctions

Represents the desired transformation from the inputs layer to the singular output (step 1)

A string that is *exactly/verbatim* *one* of:

- `"CropThenAverageRasters"`
    - Crop all raster inputs with the study area then average all inputs
- `"CropThenUnionRasters"`
    - Geometrically union all raster inputs, then crop the union with the study area
    - (union as in intersection, difference, etc)
    - https://postgis.net/workshops/postgis-intro/geometry_returning.html

![union](https://postgis.net/workshops/postgis-intro/_images/union.jpg)

- `"Slope"`
    - Union the raster inputs, treat it as a DEM and calculate the slope from the DEM
    - https://gdal.org/programs/gdaldem.html
- `"Aspect"`
    - Union the raster inputs, treat it as a DEM and calculate the aspect (orientation) from the DEM
    - https://gdal.org/programs/gdaldem.html
    - https://gdal.org/programs/gdaldem.html
- `"ResidentialProximity"`
    - Crop then union the raster inputs then calculate proximity
    - Currently hardcoded to extracting values of 22, 23, and 24 (corresponding to the CONUS land use data)
    - https://gdal.org/programs/gdal_proximity.html
- `"VectorProximity"`
    - Crop then union the vector inputs then calculate proximity
    - https://gdal.org/programs/gdal_proximity.html


### StdFunctions

Represents the transformation from the preprocessed layer to the standardized layer (step 2)

An object with two keys:

- `"function"`: *one* of the following verbatim strings:
    - `"RangeLargerBetter"`
        - A range function, linearly scaling from the minimum value to the maximum value. The maximum value will have a score of 1 (perfect suitability)
    - `"RangeSmallerBetter"`
        - A range function, linearly scaling from the minimum value to the maximum value. The maximum value will have a score of 0 (not suitabile)
    - `"SuhSigmoid"`
        - A sigmoidal function from Suh et al. (2016)
    - `"Linear"`
        - A linear interpolation between two given max and min values
    - `"Gaussian"`
        - A gaussian function from Asakereh et al. (2017)
    - `"Expr"`
        - An arbitrary expression

- `"args"`: an object whose keys depends on the function specified above [optional]. If the function was...
    - `"RangeLargerBetter"`: null (not required)
    - `"RangeSmallerBetter"`: null (not required)
    - `"SuhSigmoid"`: object with 3 keys
        - `"midpoint"`: `number`
            - The value that will receive a score of 0.5
        - `"spread"`: `number`
            - The "width" of the function
        - `"divide"`: `number` [optional]
            - An optional number to divide the input by before applying the function
    - `"Linear"`: object with 3 keys:
        - `"clamp_left"`: `number`
            - The lower clamp (clamp_left < clamp_right)
        - `"clamp_right"`: `number`
            - The upper clamp (clamp_left < clamp_right)
        - `"direction"`: verbatim `string`
            - `"LessBetter"`
                - All values less than `clamp_left` will have a score of 1, all values more than `clamp_right` will have score of 0, and values in between the clamps have scores according to a linear interpolation
            - `"MoreBetter"`
                - All values less than `clamp_left` will have a score of 0, all values more than `clamp_right` will have score of 1, and values in between the clamps have scores according to a linear interpolation
    - `"Gaussian"`: an object with 3 keys:
        - `"peak_x"`: `number`
            - The smaller value that will have a score of 1 (values greater than `peak_x` will have a score of 1)
        - `"g_midpoint"`: `number`
            - The value that will have a score of 0.5
        - `"g_divide"`: `number` [optional]
            - An optional number to divide the input by before applying the function
    - `"Expr"`: an object with 1 key:
        - `"expr"`: `string`
            - A string in numpy syntax with input/cell value represented by 'A'
            - > Calculation in numpy syntax using +, -, /, *, or any numpy array functions (i.e. log10())
            - https://gdal.org/programs/gdal_calc.html
            - Examples
                - `log10(A)`
                - `1 * logical_and(A > 500, A < 100)`


### Constraints

There is just one step when calculating constraints:

1. Process input layer(s) into a single Raster layer

A list of objects with the following keys:

- `"c_name"`: any `string`
    - The name of the constraint (can be anything)
- `"c_inputs"`: `List string` (Vector or Raster)
    - The paths to the input files
- `"c_output"`: `string`
    - The name of the output constraint file
- `"c_func"`: See [ConstraintFunc](#ConstraintFunc)

Optional keys:

- `"c_require"`: See [Require](#Require)

### ConstraintFunc

An object with two keys:

- `"function"`: *one* of the following verbatim strings:
    - `"ResidentialConstraint"`
        - Extracts values 22, 23, and 24, calculates the proximity, then apply the threshold to the proximity distances
    - `"VectorConstraint"`
        - Crop then union the vector file(s), then buffer it with the threshold and use that as the exclusion layer
    - `"ElevationConstraint"`
        - Crop then union the raster file(s), then apply the threshold to the result
    - `"AspectConstraint"`
        - Crop then union the raster file(s), then apply both thresholds to the result
    - `"SlopeConstraint"`
        - Crop then union the raster file(s), then apply the threshold to the result

- `"args"`: an object whose keys depends on the function specified above. If the function was...
    - `"AspectConstraint"`: object with 2 keys
        - `"limit1"`: `number`
            - The lower limit (between 0 and 360 degrees)
        - `"limit2"`: `number`
            - The upper limit (between 0 and 360 degrees)
    - Any other function:
        - `"distance"`: `number`
            - The buffer threshold
        - `"c_direction"`: verbatim `string`
            - `"LessBetter"`
                - All values less than `distance` will be included, the rest excluded
            - `"MoreBetter"`
                - All values less than `distance` will be excluded, the rest included
