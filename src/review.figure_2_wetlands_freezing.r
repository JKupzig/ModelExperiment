snow_threshold <- -2.0
max_degree_days <- 7.0
dregree_day_factor_wetlands <- 4.0

accum_days <- 0
snow_storage_wetland <- 0

days <- 40
precipitation <- rep(1, days)
snow_storage <- rep(0, days)
snow_melt_vector <- rep(0, days)

temp <- c(c(1,1,1), rep(-3, days/2-3), rep(1, days/2))


for (day in seq(1, days)){

  incoming_prec <- precipitation[day]
  current_temp <- temp[day]

  # snow module for local wetlands
  if (current_temp <= snow_threshold)
  {
    accum_days <- accum_days + 1
    if (accum_days > max_degree_days)
    {
      accum_days <- max_degree_days
    }
  } else {
    accum_days <- accum_days - 1
    if (accum_days < 0.0)
    {
      accum_days = 0.0
    }
  }

  if (accum_days == max_degree_days)  # frozen wetland
  {
    # accumulation of snow
    if (current_temp <= 0.0)
    {
      snow_storage_wetland <- snow_storage_wetland + incoming_prec;
      #incoming_prec = 0
    }
  }

  # melting of snow
  if (current_temp > 0)
  {
    potential_snow_melt = dregree_day_factor_wetlands * (current_temp - 0.0);
    snow_melt = min(potential_snow_melt, snow_storage_wetland);

    snow_storage_wetland <- snow_storage_wetland - snow_melt;
    #incoming_prec <- incoming_prec + snow_melt;
  }

  snow_storage[day] <- snow_storage_wetland
  snow_melt_vector[day] <- snow_melt
}

# nice plotting

to_clip <- data.frame(snow=snow_storage, temp=temp, precipitation=precipitation)
clipr::write_clip(to_clip)
plot(snow_storage, type="l", col="steelblue")
lines(temp, col="black")
lines(precipitation, col="blue")
