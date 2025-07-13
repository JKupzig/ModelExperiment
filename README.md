# ModelExperiment
 This repository supports reproducing the findings in Kupzig & Flörke (2025). It mainly consists of the compiled data and
 source-code for evaluating the data. An empty plot-folder is present so that the code can be directly excecuted if the current working directory is specified as the root of the downloaded GitHub-repository.


 ## Definition of model setups
 In the R code, the different model runs uses names following the pattern "m%i_wetland100". This names relates to the
 comparison pairs shown in Table 3 of Kupzig & Flörke (2025) in the following form:

 - m8  &#8594; S0, V0, R0
 - m9  &#8594; S1, V0, R0
 - m10 &#8594; S1, V0, R1
 - m11 &#8594; S0, V0, R1
 - m12 &#8594; S0, V1, R0
 - m13 &#8594; S1, V1, R0
 - m14 &#8594; S0, V1, R1
 - m15 &#8594; S1, V1, R1
 - m16 &#8594; S0, V2, R0
 - m17 &#8594; S1, V2, R0
 - m18 &#8594; S0, V2, R1
 - m19 &#8594; S1, V2, R1

 ## Requirements
 We've used R 4.3.1 for creating and excecuting the code. The following packages are required to excecute the code:
 - ggplot2 (3.5.1) - CRAN
 - dplyr (1.1.3) - CRAN
 - tidyr (1.3.0) - CRAN
 - RColorBrewer (1.1-3) - CRAN
 - stringr (1.5.0) - CRAN
 - WaterGAPLite (1.0.1) - GitHub
 - grDevices (4.3.1) - CRAN
 - ggh4x (0.2.8) - CRAN
 - sp (2.0-0) - CRAN
 - rgdal (1.6-7) - CRAN
 - ggspatial (1.1.9) - CRAN
 - sf (1.0-14) - CRAN

Please note that the libraries for spatial data sp and rgdal are outdated. They can be easily replaced by
using the library `terra`. However, as this work is partially done by a HPC using an older R version we've
 decided to stick to the outdated libraries for compatibility reason.

The library WaterGAPLite is only used to load the GRDC data, so the download and installation
is not mandatory to run the code. The function can be easily replaced by a self-written function for loading the GRDC files.

## Information about code: 'src'
The uploaded code is in the src-folder. Helper functions are placed in the src/helper-folder.
The naming of the files is in line with the shown figures in Kupzig & Flörke (2025).
**To create all figures used in the manuscript, create_figures.bat is provided.**

## Information of uploaded data: 'data'
This folder contains several files that are required to reproduce the findings published in Kupzig & Flörke (2025).
Note that observed discharge, i.e., GRDC data, is not part of the repository as well as the background layer
for displaying the landmass (shown in Figure 2 and A1).

The code is written in a manner that it is still
excecutable without showing the missing information. The missing information are all
open source and can be retrieved under the dollowing adresses:
- GRDC data: https://grdc.bafg.de/
- landmass-borders: https://www.naturalearthdata.com/downloads/110m-physical-vectors/110m-land/

**_cal_result_benchmarks_model_m%i_wetlStorage100.txt:_**

 This type of files contains for each of the 12 model versions different <u>evaluation metrics</u> for the calibration and validation period.  Moreover, for each basin the calibrated $\gamma$ is given. The Following metrics are included:
 - Kling-Gupta Efficiency and its components (KGE, a, b, r; Gupta el al., 2009)
 - modified KGE (Kling et al., 2012)
 - Nash-Sutcliff Efficiency (NSE; Nash & Sutcliff, 1970)
 - logarithmic version of the NSE (logNSE)

**_cal_result_discharges_model_m%i_wetlStorage100.txt:_**

This type of files contains for each of the 12 model versions the <u>simulated discharge</u> (m$^{3}$/s) of the calibrated model version for the complete period under study.

**_cal_result_model_m%i_wetlStorage100.rds:_**

This type of files contains for each of the 12 model versions the above-mentioned <u>evaluation metrics</u> for the calibration and validation period for all applied  $\gamma$-values namely, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 2.0, 3.0, 4.0, 5.0. The file can be loaded via R and the command `readRDS`.

**_cal_result_model_m%i_uncertainty.rds:_**

This type of files contains for each of the 12 model versions the above-mentioned <u>evaluation metrics</u> for the calibration and validation period using multipliers to account for uncertainties in the precipitation data and model input. The file can be loaded via R and the command `readRDS`.

**_SI_original.rds:_**

The file contains <u>signature indices</u> based on Addor et al. (2018) and listed in Table 1 of Kupzig et al. (2025). Some <u>additional information</u> is given, e.g., to discover unexpected behaviour or relate to other's results. The validation period is used to calculate the below listed metrics:
- mgn_l: Low flow magnitude [mm/d]
- mgn_h: High flow magnitude [mm/d]
- frq_l: Low flow frequency [d]
- frq_h: High flow frequency [d]
- dur_l: Low flow duration [d]
- dur_h: High flow duration [d]
- lowflow_events: Number of low flow events [-]
- highflow_events: Number of high flow events [-]
- monthly_nse: NSE evaluated with monthly aggregated data [-]
- max: Maximal peak [m$^{3}$/s]

The file can be loaded via R and the command `readRDS`.

**_SI_obs_ref_as_m8_100d_complete_validation_period.rds:_**

The signatures of the observed discharge data. The validation period is used to calculate the below listed metrics:
- mgn_l: Low flow magnitude [mm/d]
- mgn_h: High flow magnitude [mm/d]
- frq_l: Low flow frequency [d]
- frq_h: High flow frequency [d]
- dur_l: Low flow duration [d]
- dur_h: High flow duration [d]
- lowflow_events: Number of low flow events [-]
- highflow_events: Number of high flow events [-]
- monthly_nse: NSE evaluated with monthly aggregated data [-]
- max: Maximal peak [m$^{3}$/s]
- sd: standard deviation of the discharge [m$^{3}$/s]
- mean: mean of the discharge [m$^{3}$/s]

The file can be loaded via R and the command `readRDS`.

**_basin_attributes.txt:_**

The file contains <u>characteristics of the basins</u>. The following characteristics are given:
- grdc_ids: GRDC id of the basin (GRDC, 2020)
- basin_size: Area of basin in model grid [km$^{2}$]
- aridity: Aridity Index using information from Zomer & Trabucco (2022) [-]
- mean_precipitation_as_snow: Average amount of precipitation fallen below 0°C between 1979-1994 [%]
- localWetlands: With local wetland covered area in basin [%]
- globalLakes:  With global lakes covered area in basin [%]
- mean_temperature: Mean temperature in the period 1979-1994 [°C]
- sum_precipitation: Annual precipitation in the period 1979-1994 [mm]

**_overview_map.shp:_**

This is a shape-file which can be loaded by using a GIS, e.g. QGIS. It contains the <u>basin area</u> of all simulated basins, distinguishing between behavioural (defined with "1") and additional basins (defined with "2").

**_calibration_result.shp:_**

This is a shape-file which can be loaded by using a GIS, e.g. QGIS. It contains the <u>KGE value</u> for the calibration period of all simulated basins and for all simulated model setups, namely 12.

**_basin_sets.shp_**:
This is a shape-file which can be loaded by using a GIS, e.g. QGIS. It contains the <u>indication of which basin set this basin belongs to</u>. In particular:
- 0 &#8594; river
- 1 &#8594; reservoir+river
- 2 &#8594; snow+river
- 3 &#8594; snow+reservoir+river

**_basins.rds:_**
Is the flattened matrix of grid cells indicating which grid cell belongs to which basin.

**_res_types.rds:_**
Is the flattened matrix of grid cells indicating which grid cell has which reservoir type.
In particular:
- 0 &#8594; no reservoir
- 1 &#8594; irrigation reservoir
- 2-7: &#8594; non-irrigation reservoir

**_reservoir_data.csv:_**

Ths file contains information for all <u>373 simulated reservoirs</u> in the basin set. In detail, the following information is given:
- grdc-id:  GRDC id of the basin where the reservoir is located(GRDC, 2020)
- basin-set: Whether the basin is behavioural or non-behavioural
- reservoir: Whether the reservoir is irrigation or non-irrigation
- capacity_km3: The storage capacity of the reservoir (km$^{3}$)
- surface area_km2: The surface area$^{1}$ of the reservoir (km$^{2}$)
- mean depth_m: The mean depth of the reservoir calculated with `capacity / surface_area` (m)
- mean inflow_km3d: The mean inflow to the reservoir, which can be used to define whether the storage capacity is comparable low or high.

$^{1}$_Note that this information is used as integer in WaterGAP3, therefore, for small reservoirs with a surface area < 1km$^{2}$ a zero is written and within the simulation the vertical water balance of the reservoir is neglected._


 ## References
 Addor, N., Nearing, G., Prieto, C., Newman, A. J., Le Vine, N., & Clark, M. P. (2018). A Ranking of Hydrological Signatures Based on Their Predictability in Space. Water Resources Research, 54(11), 8792–8812. https://doi.org/10.1029/2018WR022606

 Gupta, H. V., Kling, H., Yilmaz, K. K., & Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, 377(1-2), 80–91. https://doi.org/10.1016/j.jhydrol.2009.08.003

 GRDC. (2020). The Global Runoff Data Centre, 56068 Koblenz, Germany.

 Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. In Journal of Hydrology 424-425, pp. 264–277. DOI: 10.1016/j.jhydrol.2012.01.011.

 Kupzig, J.; Flörke, M. (2025): A controlled model experiment for the global hydrological model WaterGAP3: Understanding recent and new advances in the model structure. In: Environmental Modelling and Software, submitted.

 Nash, J. E., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I — A discussion of principles. Journal of Hydrology, 10(3), 282–290. https://doi.org/10.1016/0022-1694(70)90255-6

 Willmott, C.  J.; Ackleson, S. G.; Davis, R. E.; Feddema, J. J.; Klink, K. M.; Legates, D. R. et al. (1985). Statistics for the evaluation and comparison of models. In J. Geophys. Res. 90 (C5), pp. 8995–9005. DOI: 10.1029/JC090iC05p08995.

Zomer, R. J., & Trabucco, A. (2022). Version 3 of the Global Aridity Index and Potential Evapotranspiration Database. Scientific Data, 9(409). https://www.nature.com/articles/s41597-022-01493-1




