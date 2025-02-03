# ModelExperiment
 This repository supports reproducing the findings in Kupzig & Flörke (2025). It mainly consists of the compiled data and
 source-code for evaluating the data. An empty plot-folder is present so that the code can be directly excecuted of the current working directory is specified as the root of the downloaded GitHub-repository.

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

Please note that the libraries for spatial data sp and rgdal are outdated. There can be easily replaced by using the more modern variant `terra`. However, as this work is partially done by a HPC using an older R version we've decided to stick to the outdated libraries for compatibility reason.

The library WaterGAPLite is only used to load the GRDC data, so the download and installation is not mandatory to run the code. The function can easily be replaced by an own function for loading GRDC data.

## Information of uploaded data
This folder contains several files that are required to reproduce the findings published in Kupzig & Flörke (2025). Note that observed discharge, i.e., GRDC data, is not part of the repository as well as the background layer for displaying the landmass (shown in Figure 1). The code is written in a manner that it is still excecutable without showing the missing information. The missing informatioon are all open source and can be retrieved under the dollowing adresses:
- GRDC data: https://grdc.bafg.de/
- landmass-borders: https://www.naturalearthdata.com/downloads/110m-physical-vectors/110m-land/

**_cal_result_benchmarks_model_m%i_wetlStorage100.txt:_**
 This type of files contains for each of the 12 model versions different <u>evaluation metrics</u> for the calibration and validation period. The Following metrics are included:
 - Kling-Gupta Efficiency and its components (KGE, a, b, r; Gupta el al., 2009)
 - modified KGE (Kling et al., 2012)
 - Nash-Sutcliff Efficiency (NSE; Nash & Sutcliff, 1970)
 - logarithmic versio of the NSE (logNSE)
 - d1-metric (Willmott, et al., 1985)
 Moreover, for each basin the calibrated $\gamma$ is given.

**_cal_result_discharges_model_m%i_wetlStorage100.txt:_**
This type of files contains for each of the 12 model versions the <u>simulated discharge</u> (m$^{3}$/s) of the calibrated model version for the complete period under study.

**_cal_result_model_m%i_wetlStorage100.rds:_**
This type of files contains for each of the 12 model versions the above-mentioned <u>evaluation metrics</u> for the calibration and validation period for all applied  $\gamma$-values namely, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 2.0, 3.0, 4.0, 5.0. The file can be loaded via R and the command `readRDS`.

**_cal_result_model_m%i_uncertainty.rds:_**
This type of files contains for each of the 12 model versions the above-mentioned <u>evaluation metrics</u> for the calibration and validation period using multipliers to account for uncertainties in the precipitation data and model input. The naming follows the convention used in the Supplementary Material of Kupzig et al. (2025). The file can be loaded via R and the command `readRDS`.

**_SI_original.rds:_**
The file contains <u>signature indices</u> based on Addor et al. (2018) and listed in Table 3 of Kupzig et al. (2025). Some <u>additional information</u> is given, e.g., to discover unexpected behaviour or relate to other's results. The validation period is used to calculate the below listed metrics:
- mgn_l: Low flow magnitude (mm/d)
- mgn_h: High flow magnitude (mm/d)
- frq_l: Low flow frequency (d)
- frq_h: High flow frequency (d)
- dur_l: Low flow duration (d)
- dur_h: High flow duration (d)
- lowflow_events: Number of low flow events (-)
- highflow_events: Number of high flow events (-)
- monthly_nse: NSE evaluated with monthly aggregated data (-)
- max: Maximal peak (m$^{3}$/s)

The file can be loaded via R and the command `readRDS`.

**_basin_attributes.txt:_**
The file contains <u>characteristics of the basins</u>. The following characteristics are given:
- grdc_ids: GRDC id of the basin (GRDC, 2020)
- basin_size: Area of basin in model grid (km$^{2}$)
- aridity: Aridity Index using information from Zomer & Trabucco (2022) (-)
- mean_precipitation_as_snow: Average amount of precipitation fallen below 0°C between 1979-1994 (%)
- localWetlands: With local wetland covered area in basin (%)
- mean_temperature: Mean temperature in the period 1979-1994 (°C)
- sum_precipitation: Annual precipitation in the period 1979-1994 (mm)

**_overview_map.shp:_**
This is a shape-file which can be loaded by using an GIS, e.g. QGIS. It contains the <u>basin area</u> of all simulated basins, distinguishing between behavioural (defined with "1") and additional basins (defined with "2").

**_reservoir_data.csv:_**
Ths file contains information for all <u>373 simulated reservoirs</u> in the basin set. In detail, the following information is given:
- grdc-id:  GRDC id of the basin where the reservoir is located(GRDC, 2020)
- basin-set: Whether the basin is behavioural or additional
- reservoir: Whether the reservoir is irrigation or non-irrigation
- capacity_km3: The storage capacity of the reservoir (km$^{3}$)
- surface area_km2: The surface area$^{1}$ of the reservoir (km$^{2}$)
- mean depth_m: The mean depth of the reservoir calculated with `capacity / surface_area` (m)
- mean inflow_km3d: The mean inflow to the reservoir, which can be used to define whether the storage capacity is comparable low or high.

$^{1}$_Note that this information is used as integer in WaterGAP3, there for small reservoirs with a surface area < 1km$^{2}$ a zero is written and wihtin the simulation the vertical water balance of the reservoir is neglected._

## Information of code
The uploaded code is in the src-folder. Helper functions are placed in the src/helper-folder. The naming of the files is in line with the shown figures in Kupzig & Flörke (2025).

 ## References
 Addor, N., Nearing, G., Prieto, C., Newman, A. J., Le Vine, N., & Clark, M. P. (2018). A Ranking of Hydrological Signatures Based on Their Predictability in Space. Water Resources Research, 54(11), 8792–8812. https://doi.org/10.1029/2018WR022606

 Gupta, H. V., Kling, H., Yilmaz, K. K., & Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, 377(1-2), 80–91. https://doi.org/10.1016/j.jhydrol.2009.08.003

 GRDC. (2020). The Global Runoff Data Centre, 56068 Koblenz, Germany.

 Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. In Journal of Hydrology 424-425, pp. 264–277. DOI: 10.1016/j.jhydrol.2012.01.011.

 Kupzig, J.; Flörke, M. (2025): A controlled model experiment for the global hydrological model WaterGAP3: Understanding recent and new advances in the model structure. In: Environmental Modelling and Software, submitted.

 Nash, J. E., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I — A discussion of principles. Journal of Hydrology, 10(3), 282–290. https://doi.org/10.1016/0022-1694(70)90255-6

 Willmott, C.  J.; Ackleson, S. G.; Davis, R. E.; Feddema, J. J.; Klink, K. M.; Legates, D. R. et al. (1985). Statistics for the evaluation and comparison of models. In J. Geophys. Res. 90 (C5), pp. 8995–9005. DOI: 10.1029/JC090iC05p08995.

Zomer, R. J., & Trabucco, A. (2022). Version 3 of the Global Aridity Index and Potential Evapotranspiration Database. Scientific Data, 9(409). https://www.nature.com/articles/s41597-022-01493-1




