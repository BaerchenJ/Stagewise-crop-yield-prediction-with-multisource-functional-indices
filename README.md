# Stagewise-crop-yield-prediction-with-multisource-functional-indices
Yield (response variable), indices (covariates) data, and scripts for reproducibility cross data preprocessing, (hyper)parameter tuning, and out-of-sample estimation.

This is the table of data sources.


      Period   |        Data        |     Source    |   URL   

    1980-2022  |        yield       |      NASS     |  https://quickstats.nass.usda.gov/

    1980-2022  |       weather      |  Daymet V4.R  |  https://www.earthdata.nasa.gov/data/catalog/ornl-cloud-daymet-daily-v4r1-2129-4.1

    2015-2022  |   soil moisture    |    SMAP V1.0  |  https://smap.jpl.nasa.gov/data/

    2015-2022  |   crop progress    |      NASS     |  https://www.nass.usda.gov/Research_and_Science/Crop_Progress_Gridded_Layers/index.php

    2018-2022  |     crop mask      |      NASS     |  https://croplandcros.scinet.usda.gov/

    2018-2022  |  satellite imagery |    HLS V2.0   |  https://hls.gsfc.nasa.gov/hls-data/

For the details of data preprocessing procedure, please refer to the manuscript Section 3 Data and study area. The scripts of data preprocessing can be found in the folder named "Data-preprocess". "ALLRUN" and "Illinois_shp" under "Data-preprocess_Illisoy" are of large size. In case you are interested in raw data preprocessing, please contact me, and I will send you these two folders.
