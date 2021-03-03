
library(data.table)
library(fst)

library(countrycode)


  dir_itgs <- "../../itgs"
  
# PREPARE DATA ------------------------------------------------------------

# Read and prep the coordinates of the countries
centroids <- fread("../hulpdata/geo_europa/europe_nuts0_centroids.csv",
  stringsAsFactors = FALSE)
# Add continents
centroids <- rbind(centroids, data.frame(
  nuts_id = c("Europe", "Americas", "Asia", "Africa", "Oceania"),
  nuts_name = c("Europe", "Americas", "Asia", "Africa", "Oceania"),
  x = c(8E6, 0E6, 1E7, 5E6, 1E7),
  y = c(3E6, 1E6, 1.5E6, 5E5, 3E5), stringsAsFactors = FALSE
))
# Coding used in this file differs from that used in ITGS
centroids$nuts_id[centroids$nuts_id == "UK"] <- "GB"
centroids$nuts_id[centroids$nuts_id == "EL"] <- "GR"
  

for (jaar in 2010:2019) {
  
  message("jaar =", jaar)

  # Read ITGS
  itgs <- fread(file.path(dir_itgs, sprintf("itgs_reference_%04d.csv.gz", jaar)))
  setnames(itgs, tolower(names(itgs)))
  # Combine non-eu countries into continents
  codes <- unique(unique(itgs$destin), unique(itgs$origin) )
  codes_eu <- codes[unique(codes) %in% centroids$nuts_id]
  itgs[, origin_eu := ifelse(origin %in% codes_eu, origin, 
    countrycode(origin, origin="iso2c", destination = 'continent'))]
  itgs[, destin_eu := ifelse(destin %in% codes_eu, destin, 
    countrycode(destin, origin="iso2c", destination = 'continent'))]
  itgs[, consign_eu := ifelse(consign %in% codes_eu, consign, 
    countrycode(consign, origin="iso2c", destination = 'continent'))]
  
  # Only select needed variables
  itgs[, obs_value := obs_value*10^unit_mult] 
  itgs <- itgs[, .(time_period, trade_type, hs6, obs_value, 
    origin_eu, destin_eu, consign_eu)]  
  # Remove missing values
  itgs <- itgs[complete.cases(itgs)]
  # Aggregate
  itgs <- itgs[, .(obs_value = sum(obs_value)), by = .(time_period, hs6, 
    origin_eu, destin_eu, consign_eu)]
  
  # Save
  dir.create("tmp_data")
  write_fst(itgs, sprintf("tmp_data/itgs_%04d.fst", jaar), compress = 100)
  

  message("Done jaar = ", jaar)
}

