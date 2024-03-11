modify_run <- function(basin_run, settings)
{
  new_run <- basin_run;
  for (name in names(settings))
  {
    new_run[[name]] <- settings[[name]]
  }
  return(new_run)
}
