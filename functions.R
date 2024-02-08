arc_to_dd <- function(x) {
  x_degrees <- str_extract(x, pattern = "[[:digit:]]*(?=°)") |> as.numeric()
  x_arcmins <- str_extract(x, pattern = "(?<=° )[[:digit:]]*(?=')")  |> as.numeric()
  x_arcsecs <- str_extract(x, pattern = "(?<=' )[[:graph:]]*(?=\\\")")  |> as.numeric()
  
  decimal_degrees <- x_arcmins / 60 + x_arcsecs / 3600
  x <- x_degrees + decimal_degrees
  x
}



# Check transitions that ended with a divergence
check_div <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n = sum(divergent)
  N = length(divergent)
  
  print(sprintf('%s of %s iterations ended with a divergence (%s%%)',
                n, N, 100 * n / N))
  if (n > 0)
    print('  Try running with larger adapt_delta to remove the divergences')
}

# Check transitions that ended prematurely due to maximum tree depth limit
check_treedepth <- function(fit, max_depth = 10) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n = length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  N = length(treedepths)
  
  print(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)',
                n, N, max_depth, 100 * n / N))
  if (n > 0)
    print('  Run again with max_depth set to a larger value to avoid saturation')
}

# Checks the energy Bayesian fraction of missing information (E-BFMI)
check_energy <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  no_warning <- TRUE
  for (n in 1:length(sampler_params)) {
    energies = sampler_params[n][[1]][,'energy__']
    numer = sum(diff(energies)**2) / length(energies)
    denom = var(energies)
    if (numer / denom < 0.2) {
      print(sprintf('Chain %s: E-BFMI = %s', n, numer / denom))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('E-BFMI indicated no pathological behavior')
  else
    print('  E-BFMI below 0.2 indicates you may need to reparameterize your model')
}

# Checks the effective sample size per iteration
check_n_eff <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  
  iter <- dim(extract(fit)[[1]])[[1]]
  
  no_warning <- TRUE
  for (n in 1:N) {
    ratio <- fit_summary[,5][n] / iter
    if (ratio < 0.001) {
      print(sprintf('n_eff / iter for parameter %s is %s!',
                    rownames(fit_summary)[n], ratio))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('n_eff / iter looks reasonable for all parameters')
  else
    print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')
}

# Checks the potential scale reduction factors
check_rhat <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  
  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      print(sprintf('Rhat for parameter %s is %s!',
                    rownames(fit_summary)[n], rhat))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('Rhat looks reasonable for all parameters')
  else
    print('  Rhat above 1.1 indicates that the chains very likely have not mixed')
}

check_all_diagnostics <- function(fit) {
  check_n_eff(fit)
  check_rhat(fit)
  check_div(fit)
  check_treedepth(fit)
  check_energy(fit)
}

# Returns parameter arrays separated into divergent and non-divergent transitions
partition_div <- function(fit) {
  nom_params <- rstan::extract(fit, permuted=FALSE)
  n_chains <- dim(nom_params)[2]
  params <- as.data.frame(do.call(rbind, lapply(1:n_chains, function(n) nom_params[,n,])))
  
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  params$divergent <- divergent
  
  div_params <- params[params$divergent == 1,]
  nondiv_params <- params[params$divergent == 0,]
  
  return(list(div_params, nondiv_params))
}

# create a bash file to download data from pcic
pcic_dl_sh <- function(
  pcic_url_catalog = "https://data.pacificclimate.org/portal/hydro_model_out/catalog/catalog.json",
    # define the type of data ie. Baseflow or Runoff
  var = "BASEFLOW",
  # define the model and scenario - should seperate
  model = "ACCESS1-0_rcp85",
  # define the location of the data directory to download to
  dir_data = "~/Dropbox/New Graph/fish-passage-22/Data/Discharge/pcic",
  # Define the bounding box of the area of interest
  lat_min = 53.32,
  lat_max = 54.89,
  lon_min = -127.53,
  lon_max = -122.92,
  # define the date range of interest
  date_start = "2019-07-13",
  date_end = "2021-10-29",
  append = FALSE){
  
  # Read the JSON data into R
  json_data <- jsonlite::fromJSON(pcic_url_catalog) 
  
  dat  <- tibble::tibble(
    name = names(json_data),
    url = unlist(json_data)
  )
  
  
  date_start = as.Date(date_start)
  date_end = as.Date(date_end)
  
  # select the url of specific model and variable
  url <- dat %>% 
    dplyr::filter(str_detect(name, model) & str_detect(name, var)) %>% 
    pull(url)
  
  # Open the netCDF file
  nc_data <- nc_open(url)
  
  # Print an overview of the file
  # print(nc_data)
  
  # Read variables
  time <- ncvar_get(nc_data, "time")
  lat <- ncvar_get(nc_data, "lat")
  lon <- ncvar_get(nc_data, "lon")
  
  # Inspect the variables
  # print(time)
  # print(lat)
  # print(lon)
  
  # Convert these dates into "days since {date_origin}"
  #extract the origin date 
  date_origin <- str_extract(nc_data$dim$time$units, "\\d{4}-\\d{1,2}-\\d{1,2}") %>% 
    as.Date()
  
  # target_dates <- as.Date(dates)
  days_start_idx <- as.numeric(date_start - date_origin)
  days_end_idx <- as.numeric(date_end - date_origin)
  
  # Find index of the value that are closest to your target lat/lon
  lat_min_idx <- which.max(lat[lat < lat_min])
  lon_min_idx <- which.max(lon[lon < lon_min])
  # we want the index from the original dataset not the index of the result
  lat_max_idx <- which(lat == min(lat[lat > lat_max]))
  lon_max_idx <- which(lon == min(lon[lon > lon_max]))
  
    # Close the ncdf4 file connection
  nc_close(nc_data)
  
  path <- paste0(path.expand(dir_data), "/", str_to_lower(var), ".nc")
  
  str <- paste0(
    "curl -o ",
    shQuote(path, type = "sh"),
    " '",
    url, ".nc?", var, 
    "'$(echo '[", 
    days_start_idx, ":", days_end_idx, "][",
    lat_min_idx, ":", lat_max_idx, "][", lon_min_idx, ":", lon_max_idx, "]'|jq -sRr @uri)"
  )
  # insert switch so that append is controlled by append param
  if(identical(append,FALSE)){
    # because our dropbox folder has a space in the path it is complex to use the path in the shell script
    # so we do it here
    dir.create(dir_data)
    
    write_lines(
      glue::glue(
        "#!/bin/bash",
        "\n",
        "set -euxo pipefail",
        "\n\n\n",
        # this is how we would create the directory in the bash file if we didn't have a wack dropbox folder name. ha
        # "mkdir -p \"{dir_data}\"",
        # "\n",
        str), 
        file = "pcic_dl.sh")
  }else(write_lines(
    paste0("\n", str), "pcic_dl.sh", append = TRUE)
  )
}
  
  
  
  
  