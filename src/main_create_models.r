
set.seed(23)

water_use_options <- c("on", "off")
waterbody_options <- c("off", "no_res", "all")
flow_velocity_options <- c("static", "variable")
snow_options <- c("snow_on_wetlands", "no_snow_on_wetlands")

complete_options <- data.frame("use"=NA, "wb"=NA, "velocity"=NA, "snow"=NA)

for (use_option in water_use_options){

  for (wb_option in waterbody_options){

    for (flow_option in flow_velocity_options){

      for (snow_option in snow_options){

        combination <- c(use_option, wb_option, flow_option, snow_option)
        complete_options <- rbind(complete_options, combination)
      }
    }
  }
}

clipr::write_clip(complete_options)
