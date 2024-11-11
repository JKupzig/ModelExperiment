
MODEL_INPUT_PATH <- r"(C:\Users\jenny\MyProject_sciebo\WaterGAPlite)"
DISCHARGE_DATA_PATH <- r"(C:\Users\jenny\MyProject_sciebo\GRDC_2020\Rohdaten)"
START <- "01.01.1980"
END <- "31.12.1989"

load_wgl_init_basin <- function(basin_info){
  basin <- init.model(
    grdc_number = basin_info$grdc_no,
    lat = basin_info$corlat,
    long = basin_info$corLong,
    cont = basin_info$cont,
    base = MODEL_INPUT_PATH
  )

  return(basin)
}
load_wgl <- function(basin_info, use_wateruse = 0){

  basin <- load_wgl_init_basin(basin_info)
  logger::log_info("basin initialized...")

  basin.climate <- init.climate(
    basin,
    start = START,
    end = END
  )
  logger::log_info("climate initialized...")

  basin.water_use <- init.wateruse(
    basin,
    sim_start = START,
    sim_end = END,
    wateruse_setting = use_wateruse
  )


  logger::log_info("water use initialized...")

  basin.run <- basin.prepare_run(basin, basin.climate, basin.water_use)
  basin.run$snow_threshold = -2
  basin.run$max_degree_days = 7

  observed_discharge <- Q.read_grdc(
    example_basin$grdc_no,
    NULL,
    cont = example_basin$cont,
    start = START,
    end = END,
    use_folder = DISCHARGE_DATA_PATH
  )

  observed_discharge_mm <- Q.convert_m3s_mmday(observed_discharge$Value, sum(basin.run$GAREA))

  return(list("run_object"=basin.run,
              "basin_object"= basin,
              "discharge_mm"=observed_discharge_mm))
}
